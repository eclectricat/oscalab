class SawOsc(var freq: SiGen = new ConstantValue(440), middlePos: SiGen, initialPhase: Float = 0f) extends CachedSiGen {

  var phase: Float = initialPhase

  var warpedPosition:Float = initialPhase

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

    val mp = middlePos.getValue(sid)

    val result = { if (phase < mp) {
      (phase/mp)
    } else {
      ((1-phase)/(1-mp))
    }}

    phase += delta_t
    if (phase > 1) phase -= 1

    if(warpedPosition > 1) warpedPosition -= 1

    2 * result - 1
  }

}

class SawOscB(var freq: SiGen = new ConstantValue(440), initialPhase: Float = 0f, blep: Boolean = true) extends CachedSiGen {
 
  var phase: Float = initialPhase


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

    var y = phase // just a ramp
    y = 2f * y - 1f

    if (blep)
       y = y - blep(phase, delta_t);

    phase += delta_t
    if (phase >= 1) phase -= 1

    return y
  }

  def  blep(t: Float, dt: Float):Float  = {
    if (t < dt) {
        val temp =  t / dt;
        return temp+temp - temp*temp - 1f;

        //val temp = t / dt - 1
        //return - t * t;
    } else if (t > 1f - dt) {
        val temp = (t - 1f) / dt;
        return temp*temp + temp+temp + 1f;

        //val temp = (t - 1) / dt + 1
        //return t * t;
    } else {
        return 0;
    }
}

}
