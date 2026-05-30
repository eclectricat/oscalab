package framework

class Portamento(inputFreq: SiGen, rate: Float, noiseAmount: Float = 0f) extends CachedSiGen {

  var currentValue = inputFreq.getValue(0)
  var quantized = inputFreq.getValue(0)
  var noise = 0f

  val r = scala.util.Random

  // designed to process frequencies as input
  // rate is the multiplier that is added at each step

  def calculateNext(sid: Int):Float = {

      val inp = inputFreq.getValue(sid)


      if (inp > currentValue) {
        currentValue = currentValue * rate
      } else {
        currentValue = currentValue / rate
      }

      val thresh = math.pow(2, (1/12f)).toFloat
      // quantization
      if ( (Math.max(currentValue, quantized) / Math.min(currentValue, quantized)) > thresh) {

        noise = noiseAmount * ( r.nextFloat() * thresh - (0.5f * thresh))
        quantized = currentValue
      }

      // at the end set it to the right value even thresh is not reached
      if ( (Math.max(currentValue, inp) / Math.min(currentValue, inp)) < thresh) {
        currentValue = inp
        quantized = currentValue
        noise = 0
      }

      return quantized *  (1 + noise)
  }

}
