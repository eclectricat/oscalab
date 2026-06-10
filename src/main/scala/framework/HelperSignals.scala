package framework

class PitchToFreq(var in:SiGen) extends SiGen {
  def getValue(sid:Int): Float = {
    Math.pow(2, in.getValue(sid) / 12).toFloat
  }
}


