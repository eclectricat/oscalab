package framework

class PulseOscOld(var freq: Int = 440) extends SiGen {

  def getValue(sid: Int): Float = {
    var sampleRate = 44100

    var relativePosition = (sid*freq.toFloat / sampleRate) % 1

    val cornerpoints = List(0.1f, 0.5f, 0.6f)
    val result =
    if (relativePosition < cornerpoints(0))
         relativePosition * (1/cornerpoints(0))
    else if  (relativePosition < cornerpoints(1))
      1
    else if  (relativePosition < cornerpoints(2)) {
      val slopewidth = (cornerpoints(2)-cornerpoints(1))
      (slopewidth - (relativePosition - cornerpoints(1))) * (1/slopewidth)
    }
    else 0

    2 * result - 1
  }

}

class PulseOsc(var freq: SiGen = new ConstantValue(440), var phaseOffset:SiGen = new ConstantValue(0f)) extends CachedSiGen {

  var relativePosition: Float = 0f

  def calculateNext(sid: Int): Float = {
    var sampleRate = 44100

    // calculate current phase based on global time
    //var relativePosition = (sid*freq.getValue(sid) / sampleRate) % 1


    // calculate current phase based on the previous phase (in case frequency changed, we want to be continuous)
    // relative position of next sample
    // for 44100 samples we have freq.getValue(sid) period
    // samples per period sampleRate/freq.getValue(sid)
    // period per sample = freq.getValue(sid)/sampleRate
    val timestep = freq.getValue(sid)/sampleRate

    val cornerpoints = List(0.01f, 0.5f, 0.51f)

    var phasedRelativePosition = relativePosition + phaseOffset.getValue(0)
    if(phasedRelativePosition > 1) phasedRelativePosition = phasedRelativePosition -1

    val result =
    if (phasedRelativePosition < cornerpoints(0))
         phasedRelativePosition * (1/cornerpoints(0))
    else if  (phasedRelativePosition < cornerpoints(1))
      1
    else if  (phasedRelativePosition < cornerpoints(2)) {
      val slopewidth = (cornerpoints(2)-cornerpoints(1))
      (slopewidth - (phasedRelativePosition - cornerpoints(1))) * (1/slopewidth)
    }
    else 0

    relativePosition += timestep
    if (relativePosition > 1) relativePosition -= 1

    2 * result - 1
  }

}

class PulseOscB(var freq: SiGen = new ConstantValue(440), var phaseOffset:SiGen = new ConstantValue(0f), val pulseWidth:SiGen = new ConstantValue(0.5f), blep: Boolean = true) extends CachedSiGen {

  var rawPhase: Float = 0f


  def calculateNext(sid: Int): Float = {
    var sampleRate = GlobalConfig.sampleRate

    // calculate current phase based on global time
    //var phase = (sid*freq.getValue(sid) / sampleRate) % 1


    // calculate current phase based on the previous phase (in case frequency changed, we want to be continuous)
    // relative position of next sample
    // for 44100 samples we have freq.getValue(sid) period
    // samples per period sampleRate/freq.getValue(sid)
    // period per sample = freq.getValue(sid)/sampleRate
    val delta_t = (freq.getValue(sid)/sampleRate)

    var phase = rawPhase + phaseOffset.getValue(sid)
    if (phase >=1) phase = phase - 1

    var y = if (phase < pulseWidth.getValue(sid)) 0f else 1f // naive pulse
    y = 2f * y - 1f

    if (blep)
       y = y - blep(phase, delta_t);

    rawPhase += delta_t
    if (rawPhase >= 1) rawPhase -= 1

    return y
  }

  def  blep(t: Float, dt: Float):Float  = {
    val pw = pulseWidth.getValue(0)
    if (t < dt) {
        val temp =  t / dt;
        return temp+temp - temp*temp - 1f;

    } else if (t > 1f - dt) {
        val temp = (t - 1f) / dt;
        return temp*temp + temp+temp + 1f;

    } else if ((t > pw) && (t < pw + dt)) { // second slope
        val temp = (t - pw) / dt;
        return  -temp-temp + temp*temp + 1f;

    } else if ((t <= pw) && (t > pw - dt)) { // second slope
        val temp = (t - pw) / dt;
        return -temp*temp - temp-temp - 1f;

    } else {
        return 0;
    }
}

}
