package framework

class Discretizr(in: SiGen, factor: Int) extends SiGen {
  override def getValue(sid: Int): Float = {
    return (in.getValue(sid) * factor).toInt / factor.toFloat
  }
}
