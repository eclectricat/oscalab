class SinOsc(var freq: SiGen = new ConstantValue(440)) extends SiGen {

  var relativePosition: Float = 0f

  def getValue(sid: Int): Float = {
    var sampleRate = 44100
    //var time:Float = sid.toFloat/sampleRate
    //return math.sin(time * math.Pi * freq.getValue(sid)).toFloat

    val timestep = freq.getValue(sid)/sampleRate

    val result = math.sin(2 * math.Pi * relativePosition).toFloat

    relativePosition += timestep
    if (relativePosition > 1) relativePosition -= 1

    return result
  }

}
