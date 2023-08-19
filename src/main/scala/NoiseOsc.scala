class NoiseOsc() extends CachedSiGen {

  /**;*/

  var b_noise: Double = 19.1919191919191919191919191919191919191919

  def calculateNext(sid: Int): Float = {

    val b_noiselast: Double = b_noise
    b_noise = b_noise + 19;
    b_noise = b_noise * b_noise;
    b_noise = b_noise + ((-b_noise + b_noiselast) * 0.5);
    val i_noise: Int = b_noise.floor.toInt;
    b_noise = b_noise - i_noise

    return b_noise.toFloat
  }

}
