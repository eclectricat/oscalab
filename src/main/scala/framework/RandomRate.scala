package framework

class RandomRate(in: SiGen, val thresh: SiGen) extends SiGen {

  var oldValue = 0f
  val r = scala.util.Random

  var phase = 0f

  //float increment = 1.f / rateReduction;
  //phase = phase + increment;
  override def getValue(sid: Int): Float = {

    val increment = (1-thresh.getValue(sid))

    val newValue = in.getValue(sid)
    //if(r.nextFloat() > thresh.getValue(sid)) {
    if(phase > 1) {
      phase = phase - 1
      oldValue = newValue
    }
    phase = phase + increment
    return oldValue
  }
}
