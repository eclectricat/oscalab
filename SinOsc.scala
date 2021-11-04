class SinOsc(var freq: Int = 440) extends SiGen {

  def getValue(sid: Int): Float = {
    var sampleRate = 44100
    var time:Float = sid.toFloat/sampleRate
    return math.sin(time * math.Pi * freq).toFloat
  }
  
}
