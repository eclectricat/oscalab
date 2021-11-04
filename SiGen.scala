trait SiGen { def getValue(sid: Int): Float

  def +(that: SiGen) = new Mixer(List(this, that))
  def *(multiplier: SiGen) = new VCA(this, multiplier)

}

class Mixer(var sources: List[SiGen]) extends SiGen {

  def getValue(sid:Int): Float = { //System.out.println(sources)
    if (sources.size == 0) return 0f
    sources.map(_.getValue(sid)).sum
  }
}

class VCA(var in: SiGen, var vol: SiGen) extends SiGen {
  def getValue(sid:Int): Float =
    vol.getValue(sid) * in.getValue(sid)
  }

class ConstantValue(value: Float) extends SiGen {
  def getValue(sid:Int): Float = value

  }

  // TODO implicit that converts float into ConstantValue if needed //import
  // scala.language.implicitConversions

  object MyImplicits {
    implicit def floatToConstantValue(f: Float):ConstantValue = { new ConstantValue(f) }
  }
