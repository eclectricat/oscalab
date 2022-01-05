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

class PulseOsc(var freq: SiGen = new ConstantValue(440)) extends CachedSiGen {

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

    relativePosition += timestep
    if (relativePosition > 1) relativePosition -= 1

    2 * result - 1
  }

}
