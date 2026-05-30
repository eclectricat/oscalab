package framework

import scala.math._

class Digital2Pole(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var v0=0f
  var v1=0f
  val eps = 0.01f

  def calculateNext(sid: Int):Float = {

    val cut =  cutoff.getValue(sid)
    val in = input.getValue(sid)
    var fb = reso.getValue(sid)
    fb =  fb + fb/(1.0f - cut + eps);
    v0 = v0 + cut  * (in - v0 + fb * (v0 - v1))
    v1 = v1 + cut  * (v0 - v1);
    return v1
  }

}

class Digital2PoleBP(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var v0=0f
  var v1=0f
  val eps = 0.01f

  def calculateNext(sid: Int):Float = {

    val cut =  cutoff.getValue(sid)
    val in = input.getValue(sid)
    var fb = reso.getValue(sid)
    fb =  fb + fb/(1.0f - cut + eps);
    v0 = v0 + cut  * (in - v0 + fb * (v0 - v1))
    v1 = v1 + cut  * (v0 - v1);
    return (v0-v1)
  }

}

class Digital2PoleHP(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var v0=0f
  var v1=0f
  val eps = 0.01f

  def calculateNext(sid: Int):Float = {

    val cut =  cutoff.getValue(sid)
    val in = input.getValue(sid)
    var fb = reso.getValue(sid)
    fb =  fb + fb/(1.0f - cut + eps);
    v0 = v0 + cut  * (in - v0 + fb * (v0 - v1))
    v1 = v1 + cut  * (v0 - v1);
    return (in-v1)
  }

}

class Chamberlin(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var v0=0f
  var v1=0f
  //val eps = 0.01f

  def calculateNext(sid: Int):Float = {

    val cut =  cutoff.getValue(sid)
    val in = input.getValue(sid)
    val res = reso.getValue(sid)

    val q1 = 2 - 2 * res
    //val f1 = (Pi * cut).toFloat
    //val f1 = (2 * sin( Pi * 0.5 * cut )).toFloat
    val f1 = cut

    val low = v1 + f1 * v0
    val hi = in - low - q1*v0
    val band = f1 * hi + v0
    //N = H + L

    // store delays
    v0 = band
    v1 = low


    return low
  }

  /**
  // parameters:
  Q1 = 1/Q
  // where Q1 goes from 2 to 0, ie Q goes from .5 to infinity

  // simple frequency tuning with error towards nyquist
  // F is the filter's center frequency, and Fs is the sampling rate
  F1 = 2*pi*F/Fs

  // ideal tuning:
  F1 = 2 * sin( pi * F / Fs )

  // algorithm
  // loop
  L = D2 + F1 * D1
  H = I - L - Q1*D1
  B = F1 * H + D1
  N = H + L

  // store delays
  D1 = B
  D2 = L

  // outputs
  L,H,B,N
  */

}




//// mystran SVF: https://www.kvraudio.com/forum/viewtopic.php?p=4913251#p4913251

class Mystran(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var z1=0.0
  var z2=0.0
  val eps = 0.01f

  def calculateNext(sid: Int):Float = {

    val cut =  min(0.99, cutoff.getValue(sid))
    val in = input.getValue(sid)
    val res = reso.getValue(sid)

    // update coeffs when parameters change
    // f = tan(M_PI*cutoff / samplerate)
    val f = tan(Pi* 0.5f * cut)

    // let's have Q go from 0.5 to inf when res goes from 0 to 1
    val Q = 0.5f / ((1-res) + eps)
    val r = f + 1 / Q

    val g = 1 / (f * r + 1)

    // rest is per sample
    // calculate outputs
    val hp = (in - r*z1 - z2) * g
    val bp = z1 + f*hp
    val lp = z2 + f*bp

    // and update state
    z1 += 2*f*hp
    z2 += 2*f*bp


    return lp.toFloat
  }
}


class SimperSVF(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var ic1eq=0.0
  var ic2eq=0.0

  def calculateNext(sid: Int):Float = {

    val cut =  min(0.99, cutoff.getValue(sid))
    val v0 = input.getValue(sid)
    val res = reso.getValue(sid)

    val g = tan(Pi* 0.5f * cut)
    val k = 2 - 2 * res

    val v1 = (ic1eq + g * (-ic2eq + v0)) / (1 + g * (g + k))
    val v2 = ic2eq + g * v1

    ic1eq = 2 * v1 - ic1eq
    ic2eq = 2 * v2 - ic2eq

    return v1.toFloat
  }
}

/**
* Sallen Key Filter modelled in modelica (linear)
* This is the version with 1 Opamp which also controls feedback
*/
class SKF_OM(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var capacitor_state=0.0
  var capacitor1_state=0.0
  System.out.println("SKF_OM - 1 OpAmpVersion")

  def calculateNext(sid: Int):Float = {

    //val cap = 0.000001 // farad
    val cap = 1 // farad

    val capacitor_C = cap
    val capacitor1_C = cap


    // R = 1 / (2 * pi * F_c * C)
    // 50HZ -> 140 ohm, nyquist: 0.318
    val cut =  min(1f, cutoff.getValue(sid))
    //val resistor = 100 - cut * 100
    //val resistor = 0.318 + 140 * (1 - cut)

    // 0.0011 smallest freq
    // 0.5 nyquist
    val targetFreq = 0.0011 + (0.5-0.0011) * cut
    val resistor = 1 / (2 * Pi * targetFreq * cap)

    val const_y = resistor

    val res =  min(1f, reso.getValue(sid))
    // potentiometer, at a ratio Rn/Rp of 2 it starts to explode
    val potentiometer_Rp = 51
    val potentiometer_Rn = res * 100 // 100: lots of reso, 0: no reso

    val signalVoltage_v = input.getValue(sid)


    val capacitor_v = (
      (2*capacitor1_C*const_y*potentiometer_Rp-potentiometer_Rn)*signalVoltage_v
      +(capacitor1_state*const_y-2*capacitor1_C*capacitor_state*scala.math.pow(const_y,2))*potentiometer_Rp
      +(capacitor_state+2*capacitor1_state)*const_y*potentiometer_Rn
    )/
    (
      (4*capacitor1_C*capacitor_C*scala.math.pow(const_y,2)+4*capacitor1_C*const_y+1)*potentiometer_Rp
      -2*capacitor_C*const_y*potentiometer_Rn
    )

    val capacitor1_v = -((capacitor1_state*const_y-capacitor_v)*potentiometer_Rp)/
    (2*capacitor1_C*const_y*potentiometer_Rp-potentiometer_Rn)

    // state updates
    //iceq[n] = −2.gc.vc[n] − iceq[n − 1]
    //gc = 2 C

    capacitor_state = -2 * 2 * cap * capacitor_v - capacitor_state
    capacitor1_state = -2 * 2 * cap * capacitor1_v - capacitor1_state

    return capacitor1_v.toFloat
  }


}

/**
* Sallen Key Filter modelled in modelica (linear)
* 2 Opamps and no feedback, for debugging purposes
*/
class SKF_OM_noFB(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var capacitor_state=0.0
  var capacitor1_state=0.0

  def calculateNext(sid: Int):Float = {

    //val cap = 0.000001 // farad
    val cap = 1
    val capacitor_C = cap


    val cut =  min(0.99, cutoff.getValue(sid))

    //val resistor = 100 + cut * 800 // 100 to 900 ohm
    val resistor = 100 - cut*100
    val r_Resistor_R = resistor

    val signalVoltage_v = input.getValue(sid)

    // no fb case
    val capacitor_v = (signalVoltage_v-capacitor_state*r_Resistor_R)/(2*capacitor_C*r_Resistor_R+1)
    val capacitor1_v = -(capacitor1_state*r_Resistor_R-capacitor_v)/(2*capacitor_C*r_Resistor_R+1)

    capacitor_state = -2 * 2 * capacitor_C * capacitor_v - capacitor_state
    capacitor1_state = -2 * 2 * capacitor_C * capacitor1_v - capacitor1_state

    return capacitor1_v.toFloat
  }


}

/**
* Sallen Key Filter modelled in modelica (linear)
* This is the version with 2 Opamp, plus one for the feedback control
*/
class SKF_OM_FB(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var capacitor_state=0.0
  var capacitor1_state=0.0

  def calculateNext(sid: Int):Float = {

    //val cap = 0.000001 // farad
    val cap = 1
    val capacitor_C = cap

    val cut =  min(1, cutoff.getValue(sid))

    //val resistor = 100 - cut*100
    //val resistor = 0.318 + 140 * (1 - cut)

    val targetFreq = 0.001 * pow(2,9*cut) // exponential mapping from cutorr param to f_c/f_s
    lazy val resistor = 1 / (2 * Pi * targetFreq * cap) // calculate res from frequency

    val r_Resistor_R = resistor

    val signalVoltage_v = input.getValue(sid)

    val res =  min(1f, reso.getValue(sid))
    // starts to explode when r2/r3 is 1
    val r_Resistor2_R = res * 100 // 90
    val r_Resistor3_R = 100

    val capacitor_v = -(
      r_Resistor_R*(r_Resistor3_R*(-2*capacitor_C*signalVoltage_v-capacitor1_state)+(-capacitor_state-capacitor1_state)*r_Resistor2_R)
      +r_Resistor2_R*signalVoltage_v+2*capacitor_C*capacitor_state*r_Resistor3_R*scala.math.pow(r_Resistor_R,2)
    )/(
      4*scala.math.pow(capacitor_C,2)*r_Resistor3_R*
      scala.math.pow(r_Resistor_R,2)+(2*capacitor_C*r_Resistor3_R-2*capacitor_C*r_Resistor2_R)*r_Resistor_R+r_Resistor3_R
    )

    val capacitor1_v = -(capacitor1_state*r_Resistor3_R*r_Resistor_R-capacitor_v*r_Resistor3_R
    )/(2*capacitor_C*r_Resistor3_R*r_Resistor_R-r_Resistor2_R)


    capacitor_state = -2 * 2 * capacitor_C * capacitor_v - capacitor_state
    capacitor1_state = -2 * 2 * capacitor_C * capacitor1_v - capacitor1_state

    return capacitor1_v.toFloat
  }


}

/**
* Sallen Key Filter modelled in modelica, with diodes in feedback path
* This is the version with 2 Opamp, plus one for the feedback control
*/
class SKF_OM_Diodes(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  var capacitor_state=0.0f
  var capacitor1_state=0.0f

  val PiF = Pi.toFloat

  var estimate = 0.1f // the estimate for the nonlinear optimisation. use the value from the timestep as initial value

  // processing statistics
  var totalIters = 0
  var totalSubIters = 0

  // variables
  val cap: Float = 1
  val capacitor_C: Float = cap
  // the settings of the diode
  val diode1_Vt:Float = 0.04f * 5 // 0.04
  val diode1_Ids:Float = (1e-6).toFloat

  var r_Resistor_R:Float = 0f
  var signalVoltage_v: Float = 0f
  var res: Float =  0f
  var r_Resistor2_R:Float = 0f
  var r_Resistor3_R:Float = 0f
  // precompute some things
  var powC2 = 0f
  var powR3_4 = 0f
  var powR_2 = 0f
  var powR2_3 = 0f

  // oversampling
	var inTminus1 = 0f; // previour input sample, needed for linear interpolation in oversampling
	val osFactor = 1 // oversampling doesn't change much. 

  def calculateNext(sid: Int):Float = {

    //val cap = 0.000001 // farad

    if ((sid % 100 == 0) || (r_Resistor_R==0))
    {
      val cut: Float =  min(1, cutoff.getValue(sid))
      //val resistor = 100 - cut*100
      //lazy val resistor: Double = 0.318 + 140 * (1 - cut)
      //val targetFreq = 0.0011 + (0.5-0.0011) * cut // linear mapping from cutoff param to f_c/f_s
      var targetFreq = 0.001f * pow(2,9*cut).toFloat // exponential mapping from cutoff param to f_c/f_s
      targetFreq = targetFreq / osFactor

      val resistor = 1 / (2 * PiF * targetFreq * cap) // calculate res from frequency
      r_Resistor_R = resistor

      val res: Float =  min(1f, reso.getValue(sid))
      // starts to explode when r2/r3 is 1
      r_Resistor2_R  = res * 100 + 1 // + 1 to avoid division by 0
      r_Resistor3_R = 100

      // precompute some things
      powC2 = pow(capacitor_C,2).toFloat
      powR3_4= pow(r_Resistor3_R,4).toFloat
      powR_2 = pow(r_Resistor_R,2).toFloat
      powR2_3 = pow(r_Resistor2_R,3).toFloat
    }
  //signalVoltage_v = input.getValue(sid)
  val in = input.getValue(sid)

  // estimating diode1_v with newton method
  //estimate = 0.0 // initial estimate, comment out to use the one from the last iteration

  var capacitor_v = 0f
  var capacitor1_v = 0f

  // oversampling
  for (oi <- 1 to (osFactor)) {

    val oversampledInput = inTminus1 + (oi/osFactor.toFloat) * (in - inTminus1)
    signalVoltage_v = oversampledInput

  var (residue, gradient) = evalNonlinearFunction(estimate)
  var numIterations = 0

  var newEstimate = estimate //+ 0.001 // perturb in case gradient is zero

  while ((numIterations < 50) && (abs(residue) > 0.001f)) {
    //if (gradient != 0)
    newEstimate = estimate - residue/gradient

    /**
    if (abs(gradient) < 0.001)
      newEstimate=newEstimate + 0.001
    else
      newEstimate = estimate - residue/gradient
      */

    var residue_gradient = evalNonlinearFunction(newEstimate)
    var newResidue = residue_gradient._1 // WTF
    var newGradient = residue_gradient._2

    var subiter = 0
    var stepSize = 0.5f
    while((abs(newResidue) > abs(residue)) && (subiter < 10)) { //make a smaller step
      subiter += 1
      totalSubIters += 1
      newEstimate = estimate - stepSize * (residue/gradient)
      stepSize = stepSize * 0.5f

      residue_gradient = evalNonlinearFunction(newEstimate)
      newResidue = residue_gradient._1 // WTF
      newGradient = residue_gradient._2
      //System.out.println("subiter")
    }

    estimate = newEstimate
    residue = newResidue
    gradient = newGradient

    numIterations += 1
    totalIters += 1
  }

  capacitor_v = getCap(estimate)
  capacitor1_v = getCap1(estimate, capacitor_v)

  capacitor_state = -2 * 2 * capacitor_C * capacitor_v - capacitor_state
  capacitor1_state = -2 * 2 * capacitor_C * capacitor1_v - capacitor1_state

  if((sid % 100000 == 0)&& (oi == 1)) {
    System.out.println("Iterations per sample: "+ totalIters/100000f)
    System.out.println("Subiterations per sample: "+ totalSubIters/100000f)
    totalIters = 0
    totalSubIters = 0
  }

} // oversampling

  inTminus1 = in

  return capacitor1_v.toFloat
  //return estimate.toFloat
} // calculate Next

def evalNonlinearFunction(estimate: Float): (Float,Float) = {

  val diode_v = -estimate // this should have been elimiated (?)
  val diode1_v = estimate

  val expD = exp(diode_v/diode1_Vt).toFloat
  val expD1 = exp(diode1_v/diode1_Vt).toFloat

  val inter1 = 2f * capacitor_C * r_Resistor2_R * r_Resistor_R

  val grad1: Float =( -powR2_3 * powR3_4
  * ( r_Resistor3_R * (
    (diode1_Ids * expD1 * r_Resistor2_R *(-4f * powC2 * pow(r_Resistor_R,2).toFloat - 2f * capacitor_C * r_Resistor_R - 1f)) / diode1_Vt
    - 4f * powC2.toFloat * powR_2
    - 2f * capacitor_C * r_Resistor_R - 1f)
    + inter1
  ))

  val residue1: Float = (-powR2_3*powR3_4*(r_Resistor2_R*(-signalVoltage_v+2*capacitor1_state*capacitor_C*powR_2
  + 2*capacitor_C*diode1_v*r_Resistor_R+(capacitor_state+capacitor1_state)*r_Resistor_R)+r_Resistor3_R*(r_Resistor2_R*(diode1_Ids *
    expD*(4*powC2*powR_2+2*capacitor_C*r_Resistor_R+1)+diode1_Ids*expD1 *
    (-4f * powC2*powR_2-2*capacitor_C*r_Resistor_R-1))+diode1_v
    * (-4f * powC2*powR_2-2*capacitor_C*r_Resistor_R-1))) )

    return (residue1, grad1)
  }

  def getCap(diode1_v: Float): Float = {
    val diode_v = -diode1_v

    return (
      (r_Resistor2_R*signalVoltage_v-capacitor_state*r_Resistor2_R*r_Resistor_R
        +((diode1_Ids*exp(diode1_v/diode1_Vt).toFloat-diode1_Ids*exp(diode_v/diode1_Vt).toFloat)*r_Resistor2_R+diode1_v)*r_Resistor3_R
        +diode1_v*r_Resistor2_R
      )
      /
      (2*capacitor_C*r_Resistor2_R*r_Resistor_R+r_Resistor2_R))
    }

    def getCap1(diode1_v: Float, cap_v: Float): Float = {
      val diode_v = -diode1_v

      return  ( -(capacitor1_state*r_Resistor2_R*r_Resistor_R+
        ((diode1_Ids * exp(diode1_v/diode1_Vt).toFloat -diode1_Ids * exp(diode_v/diode1_Vt).toFloat) *r_Resistor2_R+diode1_v)*r_Resistor3_R+(diode1_v-cap_v)*r_Resistor2_R
      ) / (2*capacitor_C*r_Resistor2_R*r_Resistor_R+r_Resistor2_R))
    }

    // alternative, using other equations
    /**def getCap1_(diode1_v: Double): Double = {
    val diode_v = -diode1_v

    return ((((diode1_Ids*exp(diode_v/diode1_Vt)-diode1_Ids*exp(diode1_v/diode1_Vt))*r_Resistor2_R-diode1_v)*r_Resistor3_R)
    /r_Resistor2_R)
  }
  def getCap_(diode1_v:Double, cap1_v:Double): Double = {
  val diode_v = -diode1_v

  return (((2*cap1_v*capacitor_C+capacitor1_state)
  *r_Resistor2_R*r_Resistor_R
  +((diode1_Ids*exp(diode1_v/diode1_Vt)-diode1_Ids*exp(diode_v/diode1_Vt))*r_Resistor2_R+diode1_v)
  *r_Resistor3_R+(diode1_v+cap1_v)*r_Resistor2_R
)/r_Resistor2_R)
}*/

def fexp(x:Double):Double = {
  val tmp: Long = (1512775 * x).toLong + (1072693248 - 60801)
  return java.lang.Double.longBitsToDouble(tmp << 32)
}

}
