package framework

class SinOsc(var freq: SiGen = new ConstantValue(440), var phaseOffset: SiGen = new ConstantValue(0f)) extends CachedSiGen {

  var relativePosition: Float = 0f

  def calculateNext(sid: Int): Float = {
    var sampleRate = 44100
    //var time:Float = sid.toFloat/sampleRate
    //return math.sin(time * math.Pi * freq.getValue(sid)).toFloat

    val timestep = freq.getValue(sid)/sampleRate

    val result = math.sin(2 * math.Pi * relativePosition + phaseOffset.getValue(sid)).toFloat

    relativePosition += timestep
    if (relativePosition > 1) relativePosition -= 1

    return result
  }

}
