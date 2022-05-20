

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

class WaveFolderSin(in: SiGen, scaling: SiGen) extends SiGen {

  override def getValue(sid: Int): Float = {
    var value = in.getValue(sid) * scaling.getValue(sid)
    value = Math.sin(value+0.5f).toFloat
    //value = value * value
    return value
  }
}

class WaveFolderSqueeze(in: SiGen, scaling: SiGen) extends SiGen {

  override def getValue(sid: Int): Float = {
    var value = in.getValue(sid) //* scaling.getValue(sid)

    val s = scaling.getValue(sid)
    val offset = 0.5f * (s - 1)


    if (value > offset) {
      value = value - offset
    } else {
      //value = value + offset
      if (value > offset/2) value = (offset/2) - (value - offset/2)
    }
    //value = Math.sin(value+0.5f).toFloat
    //value = value * value
    return value
  }
}
