class Env(atk: Float, dec: Float, sus:Float, rel: Float, var voiceController: Option[EnvCallbackDestination], var callbackIdentifier: Int) extends SiGen {

  var state = 1
  // 0: not started
  // 1: attack phase
  // 2: decay
  // 3: sustain
  // 4: release
  // 5: finito

  var value: Float = 0f
  var sampleRate = 44100
  val epsilon = 2f // to make sure there no division by zero

  def getValue(sid: Int): Float = {
    state match {
      case 1 => {
        // increase by one in atk seconds,
        val increment = 1 / (atk * sampleRate + epsilon)
        value += increment
        if (value >= 1.0) {
          value = 1.0f
          state = 2
        }
      }

      case 2 => {
        val increment = (1-sus) / ( dec * sampleRate + epsilon)
        value -= increment
        if (value <= sus) state = 3
      }

      case 4 =>
        val increment = 1 / ( rel * sampleRate + epsilon)
        value -= increment
        if (value <= 0) { state = 5; voiceController.map(_.envelopeDone(callbackIdentifier)) }

      case _ =>
    }

    return math.min(math.max(0, value),1)
  }

  def release() = {
    state = 4
  }

  def retrigger() = {
    state = 1
  }

}

trait EnvCallbackDestination {
  def envelopeDone(callbackIdentifier: Int)
}
