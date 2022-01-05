class SawOsc(var freq: SiGen = new ConstantValue(440), middlePos: Float) extends CachedSiGen {

  var relativePosition: Float = 0f

  def calculateNext(sid: Int): Float = {
    var sampleRate = GlobalConfig.sampleRate

    // calculate current phase based on global time
    //var relativePosition = (sid*freq.getValue(sid) / sampleRate) % 1


    // calculate current phase based on the previous phase (in case frequency changed, we want to be continuous)
    // relative position of next sample
    // for 44100 samples we have freq.getValue(sid) period
    // samples per period sampleRate/freq.getValue(sid)
    // period per sample = freq.getValue(sid)/sampleRate
    val timestep = freq.getValue(sid)/sampleRate

    val result = { if (relativePosition < middlePos) {
      (relativePosition/middlePos)
    } else {
      ((1-relativePosition)/(1-middlePos))
    }}

    relativePosition += timestep
    if (relativePosition > 1) relativePosition -= 1

    2 * result - 1
  }

}
