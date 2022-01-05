trait SiGen {
  // this is the minimum that needs to be implemented in every sublass
  def getValue(sid: Int): Float

  // override this if there should be some stereo/multichannel behaviour
  // default is just the mono behaviour
  def getValue(sid:Int, channel:Int): Float = {
    return getValue(sid)
  }


  def +(that: SiGen) = new Mixer(List(this, that))
  def *(multiplier: SiGen) = new VCA(this, multiplier)

}

/**
SiGen with some functionality to avoid that the same sample is calculated multiple times
This is for modules with an internal state (related to time). Otherwise, asking for the sample twice would result in twice the fewquency for an osc e.g.
This also helps to save processing cost - don't propagate calls along the graph if the value has been calculated before
Note: for now this is only Mono
**/
trait CachedSiGen extends SiGen {

  var latestSid: Int = -1
  var latestValue: Float = 0f

  override def getValue(sid: Int) = {
    if (sid > latestSid) {
      latestValue = calculateNext(sid)
      latestSid = sid
    }
    latestValue
  }

  def calculateNext(sid: Int): Float

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

class ConstantValue(var value: Float) extends SiGen {
  def getValue(sid:Int): Float = value

}

class PanBalance(var in: SiGen, var position: SiGen) extends SiGen {
  override def getValue(sid:Int, channel:Int): Float =  channel match {

    case 0 =>
      val level = (-1 * position.getValue(sid) + 1) / 2.0f
      in.getValue(sid, channel) * level
    case 1 =>
    val level = (position.getValue(sid) + 1) / 2.0f
      in.getValue(sid, channel) * level
    case _ => 0.0f
  }

  def getValue(sid:Int): Float = getValue(sid, 0)

}

  // TODO implicit that converts float into ConstantValue if needed //import
  // scala.language.implicitConversions

  object MyImplicits {
    implicit def floatToConstantValue(f: Float):ConstantValue = { new ConstantValue(f) }
  }
