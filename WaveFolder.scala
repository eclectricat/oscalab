

class WaveFolder(in: SiGen, scaling: SiGen, numStages: Int = 1) extends SiGen {

  override def getValue(sid: Int): Float = {
    var value = in.getValue(sid) * scaling.getValue(sid)

    for (i<-0 until numStages) {
      if (value > 1)
        value = 1 - (value -1)
      else if (value < -1)
        value = -1 - (value + 1)
      else return value
    }

    return value
  }
}
