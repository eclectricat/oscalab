package framework

class LinEnv(var atk: Float, var dec: Float, var sus:Float, var rel: Float, var voiceController: Option[EnvCallbackDestination], var callbackIdentifier: Int) extends CachedSiGen with Env {

  var state = 1
  // 0: not started
  // 1: attack phase
  // 2: decay
  // 3: sustain
  // 4: release
  // 5: finito

  var value: Float = 0f
  var sampleRate = 44100
  val epsilon = 20f //2f // to make sure there no division by zero

  def calculateNext(sid: Int): Float = {
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
        //val increment = (1-sus) / ( dec * sampleRate + epsilon)
        val increment = 1 / ( dec * sampleRate + epsilon)
        value -= increment
        if (value <= sus) state = 3
      }

      case 4 =>
        val increment = 1 / ( rel * sampleRate + epsilon)
        value -= increment
        if (value <= 0) { state = 5; voiceController.map(_.envelopeDone(callbackIdentifier)) }

      case _ =>
    }

    // shape the linear env into something that looks like an exponential
    val expFactor = 1f //6f
    val expValue = if(state == 1) scala.math.pow(value,1f/expFactor) else scala.math.pow(value,expFactor)
    //val expValue = if(state == 1) value else scala.math.pow(value,expFactor)

    return math.min(math.max(0, expValue.toFloat),1)
  }

  def release() = {
    state = 4
  }

  def retrigger() = {
    state = 1
  }

  def isDone() = state == 5

  def canBeKilled() = isDone() || voiceController.isEmpty

}

class ExpEnv(var atk: SiGen, var dec: SiGen, var sus:SiGen, var rel: SiGen, var voiceController: Option[EnvCallbackDestination], var callbackIdentifier: Int) extends CachedSiGen with Env{

  var state = 1
  // 0: not started
  // 1: attack phase
  // 2: decay
  // 3: sustain
  // 4: release
  // 5: finito

  var value: Float = 0f
  var sampleRate = 44100
  val epsilon = 5f // 200f //2f // to make sure there no division by zero

  // assuming atk, dec and so on specify the time it takes until the signal is 0.001
  val targetLevel = 0.001f
  var fDecay = scala.math.pow(targetLevel, 1f/(dec.getValue(0)*GlobalConfig.sampleRate)).toFloat
  var fRelease = scala.math.pow(targetLevel, 1f/(rel.getValue(0)*GlobalConfig.sampleRate)).toFloat

  def recalculateExpFactors() = {
    fDecay = scala.math.pow(targetLevel, 1f/(dec.getValue(0)*GlobalConfig.sampleRate)).toFloat
    fRelease = scala.math.pow(targetLevel, 1f/(rel.getValue(0)*GlobalConfig.sampleRate)).toFloat
  }

  def calculateNext(sid: Int): Float = {
    state match {
      case 1 => {
        // increase by one in atk seconds,
        val increment = 1 / (atk.getValue(0) * sampleRate + epsilon)
        value += increment
        if (value >= 1.0) {
          value = 1.0f
          state = 2
        }
      }

      case 2 => {
        //val increment = (1-sus) / ( dec * sampleRate + epsilon)
        val delta = (value - sus.getValue(0)) * fDecay
        value = sus.getValue(0) + delta
        // never actually switch officially to the sustain phase...

        //value = value * fDecay
        //if (value <= sus) state = 3
      }

      case 4 =>
        value = value * fRelease
        if (value <= targetLevel) {
          state = 5;
          value = 0;
          voiceController.map(_.envelopeDone(callbackIdentifier)) }

      case _ =>
    }

    return math.min(math.max(0, value),1)
  }

  def release() = {
    state = 4
  }

  def retrigger() = {
    recalculateExpFactors()
    state = 1
  }

  def isDone() = state == 5

  def canBeKilled() = isDone() || voiceController.isEmpty

}

trait EnvCallbackDestination {
  def envelopeDone(callbackIdentifier: Int)
}

trait Env {
  def release()
  def retrigger()
  def isDone(): Boolean
  def canBeKilled(): Boolean // if it has no callback (i.e. not relevant for ending the note) => true. otherwise true if IsDone()
}
