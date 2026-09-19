package framework


import framework.MyImplicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Random
// ms.p(rythmPattern = List(1)).cutoff(List(0.5)).frequency(List(100,200))


object LiveImplicits {
  implicit def parameterToConstantValue(p: Parameter): ConstantValue =
    p.signal

  implicit def ConstantValueToParameter(value: ConstantValue): Parameter =
    Parameter.wrapExisting(value)

  implicit def pairToPair(p: (ConstantValue, List[LockValue])): (Parameter, List[LockValue]) =
    (Parameter.wrapExisting(p._1), p._2)

  implicit def floatToFloatLock(value:Float): FloatLock = FloatLock(value)

  implicit def intToPatternElement(value: Int): PatternElement = {
    PatternElement.trig(value)
  }
}

abstract class SoundSource {

  var triggerables: List[Triggerable] = List()

  def engage() = triggerables.foreach(_.retrigger())
  def release() = triggerables.foreach(_.release())

  // optional if a ss wants to implement probability
  def prob(): Float= 1

  // sequencer lanes can store their locks here, these params will be unlocked before every new trig
  var lockedParams: List[Parameter] = List()


  // efficient syntax to create a playable
  def p(pattern: Pattern): Playable =
    new Playable(this, pattern, Map())

  def p(listp: List[Int]): Playable = {
    val pattern = ListPattern.fromIntList(listp)
    this.p(pattern)
  }

}

/**
 * Parameter: wrapper around ConstantValue
 * - can be locked, unlocked
 * - has a name and range
 *
 */
class Parameter(value:Float, var min:Float, var max:Float, var name:String="Unnamed") {
// TODO: check bounds
  var locked = false
  var originalValue:Float = value
  var signal: ConstantValue = new ConstantValue(value)

  def lock(lockValue:Float) = {

    if (!locked) originalValue = signal.value
    signal.value = lockValue
    locked = true
  }

  def unlock() = {
    if (locked) {
      signal.value = originalValue
      locked = false
    }
  }

  def set(newValue:Float) = {
    if (locked) originalValue = newValue
    else signal.value = newValue
  }

  def get() = signal.value

  def getNormalValue() =
     if (locked) originalValue else signal.value

}

object Parameter {

  def wrapExisting(value: ConstantValue, min: Int = -10000, max: Int = 10000): Parameter = {
    val p = new Parameter(0, min, max)
    p.signal = value // todo: build constructor for this directly
    return p
  }
}


// make an explicit class here, but also try with anonymous classes later
class Sound(mixer: Mixer) extends SoundSource {

  val freq = new ConstantValue(110f)
  val cutoff = new Parameter(0.5f, 0f, 1f, "Cutoff")
  val reso = new Parameter(0.8f, 0f, 1f, "Reso")
  val osc = new SawOscB(freq)
  val env = new ExpEnv(0f, 1f, 0f, 1f, None, 0)
  val filter = new SKF_OM_Diodes(osc * 1f, cutoff.signal + env * 0.2f, reso.signal)

  val out: VCA = filter * env
  val out2 = new WaveShaper(out, 15f) * 0.3f

  // add connect the audio output
  mixer.sources = out2 :: mixer.sources

  // make sure it can be triggered by the sequencer
  triggerables = env :: triggerables

  val prob_ = new Parameter(1f, 0, 1, "SoundProb")
  override def prob() = prob_.get()


}

case class PatternElement(
                           trigLength: Option[Int] = None,
                           locks: Map[Parameter, Float] = Map.empty,
                           prob: Option[Float] = None,
                           //unlockLocks: Boolean = true, // will this trig unlock locked params (NIP)
                         ) extends NestablePatternElement{
  // maybe use something that is not ambiguous when used on an int
  def %%(p: Float): PatternElement = {
    this.copy(prob = Some(p))
  }

  def prob(p:Float): PatternElement = {
    this.copy(prob = Some(p))
  }

  def lock(kv: (Parameter, Float)): PatternElement = {
    copy(locks = locks + kv)
  }

}

object PatternElement {
  def x = new PatternElement()
  def trig(length: Int) = new PatternElement(trigLength = Some(length))
}


trait Pattern extends NestablePatternElement {
  def getNext(counter: Int): PatternElement
}

// just to make it nestable
trait NestablePatternElement

case class ListPattern(var elements: List[NestablePatternElement]) extends Pattern{

  def getNext(counter: Int) : PatternElement = {
    val index = counter % elements.length

    elements(index) match {
      case pattern: Pattern =>
        return PatternElement.x // TODO: handle the nested case _: framework.Pattern => ???
      case p: PatternElement => return p
      //case _ => ???
    }
  }
}

object ListPattern {
  def fromIntList(elements: List[Int]): ListPattern = {
      new ListPattern(elements.map(i => PatternElement.trig(i))
    )
  }

  // move that to LiveImplicits
  /*implicit def intToPatternElement(value: Int): PatternElement = {
    PatternElement.trig(value)
  }*/

  def apply(elements : PatternElement*): ListPattern = {
    ListPattern(elements.toList) // using the constructor of the case class
  }
}

/**
 * For the parameter locks in the separate lists
 */

trait LockValue

object NoLock extends LockValue
case class FloatLock(value: Float) extends LockValue

 // note: there is an implicit conversion from Float to FloatLock in the LiveImplicits
 // but this does not work for lists, therefore I need this:
object Locks {
  def apply(values: LockValue*): List[LockValue] =
    values.toList
}


/**
 * All the information a sequencer needs to play
 */
class Playable(val s: SoundSource,
               val p: Pattern,
               var mods: Map[Parameter, List[LockValue]])  {

  // nice syntax to add parameter automation
  def mod(pair: (Parameter, List[LockValue])) = {
    mods += pair
    this
  }

  // maybe this can be done with the implicit conversion, or maybe it goes wrong
  // can not really be done because of type erasure (TODO)
  def mod2(pair: (ConstantValue, List[LockValue])) = {
    val tempParam:Parameter = Parameter.wrapExisting(pair._1)
    val newPair = (tempParam, pair._2)
    mods += newPair
    this
  }



  /**
   * Check if it is a ConstantValue, and if yes, modulate it
   * @param pair of signal to modulate and Sequence of values
   * @return this
   */
  def modMaybe(pair: (SiGen, List[LockValue])): Playable = {
    pair._1 match {
      case value: ConstantValue => {
        mod2((value, pair._2))
      }
      case _ => this
    }
  }

}

class SequencerLane(var ply: Playable) {
  var counter: Int = 0 // how many ticks have passed
  var playcounter: Int = 0 // incremented whenever a note is played

  var muted = false
  var noteOn = false

  //var locked: List[Parameter] = List()

  def tick(): Unit = {
    //val index = counter % ply.p.length

    if (noteOn) {
      ply.s.release() // TODO: only release when note length is done
      noteOn = false
    }

    ply.p.getNext(counter) match {
      case PatternElement(None, _, _) => {
        // No trigger
      }

      case PatternElement(Some(0), _, _) => {
        // No trigger
        // TODO: this case could cut off a note that is playing (not just release(), but actually cut it, TBD)
      }

      case PatternElement(Some(length), triglocks, prob) => {

        // unlock even if muted (and when conditional note does not play)
        //locked.foreach(_.unlock())
        //locked = List()
        ply.s.lockedParams.foreach(_.unlock())
        ply.s.lockedParams = List()

        if (!muted) {
          // locks that are part of the trig



          val trigLocked = triglocks.map { case (p,v) =>
            p.lock(v)
            p
          }.toList

          // additional mods (those that are not part of the PatternElement)
          val additionalLocked = ply.mods.map { case (param, locklist) =>
            val modindex = playcounter % locklist.length
            //tuple._1.value = tuple._2(modindex)
            locklist(modindex) match {
              case FloatLock(value) => {
                param.lock(value)
                Some(param)
              }
              case NoLock => None
              case _ => None
            }
          }.toList.flatten

          ply.s.lockedParams = trigLocked ++ additionalLocked

          // do we _actually_ want to trigger the note?
          val prob_ = prob.getOrElse(ply.s.prob())
          if (Random.nextFloat() <= prob_) {
            noteOn = true
            ply.s.engage()
          }
        }
        playcounter += 1 // increase playcounter also when muted, so it stays in sync
      }

    }

    counter += 1
  }

  def reset(): Unit = {
    counter = 0;
    playcounter = 0;
  }

}

class Sequencer(var bpm:Int = 120) {
  var lanes : Map[Any, SequencerLane] = Map() //List[SequencerLane] = List()
  var running = false

  def set(laneId: Any, playable: Playable) = {
    if (!lanes.contains(laneId)) {
      // TODO: new lane counters should be synchronised according to length
      val lane = new SequencerLane(playable)
      lanes =   lanes + (laneId -> lane)
    } else {
      lanes.get(laneId).foreach(_.ply = playable)
    }
  }

  def mute(laneId: Any) = lanes.get(laneId).foreach(_.muted = true)
  def unmute(laneId: Any) = lanes.get(laneId).foreach(_.muted = false)

  def solo(laneIds: List[Any]) =
  {
    lanes.foreach{lane =>
      if (!laneIds.contains(lane._1))
        lane._2.muted = true
      else
        lane._2.muted = false
    }
  }

  def addLane(laneId: Any, lane: SequencerLane): Unit = {
    lanes =   lanes + (laneId -> lane)
  }

  def start() = {
      running = true

      lanes.values.foreach(_.reset())

      Future{run()}
  }

  def stop() = {
    running = false
    lanes.values.foreach { l =>
      //l.locked.foreach(_.unlock())
      l.ply.s.lockedParams.foreach(_.unlock())
      //l.locked = List()
      l.ply.s.lockedParams = List()
      l.ply.s.release()
    }
  }

  def run(): Unit = {

    while(running) {
      lanes.values.foreach(_.tick())
      //System.out.println("tick")

      // calculate pause (POC)
      // we want seconds per beat
      // 'beat' are quarters, we want 16ths
      val bps = bpm / 60.0
      val spb = 1 / bps
      val sec_per_16th = spb / 4
      Thread.sleep((sec_per_16th * 1000).toLong)
    }
  }

}

class AlgoPattern(algo: FillAlgo, ss: SoundSource) extends Pattern {

  override def getNext(counter: Int): PatternElement = {
    algo.getNext(counter, ss)
  }
}

// to the the algo everything important about the Playable
case class PlayableInfo(playable:Playable,
                        weight: SiGen = new ConstantValue(1f),
                        volume: Option[Parameter] = None,
                        stutter: Option[Stutter] = None,
                        bitred: Option[Parameter] = None,
                       ) {

  val stutEnable: Option[Parameter] = stutter.map(_.enable) match {
    case Some(cv:ConstantValue) => Some(Parameter.wrapExisting(cv, 0, 1))
    case _ => None
  }

  val stutLoopEnd: Option[Parameter] = stutter.map(_.loopEnd) match {
    case Some(cv:ConstantValue) => Some(Parameter.wrapExisting(cv, 0, 1))
    case _ => None
  }


}


class FillAlgo(currentPlayableInfo: List[PlayableInfo]) {

  val currentPlayables = currentPlayableInfo.map(_.playable)

  val fillProb =  new Parameter(0.5f, 0, 1f, "FillProb")

  var lastComputedStep: Int = -1

  // this is the sound that the filler also plays, if any (only one sound at a time, for the whole list of playables)
  var decidedTrig: Option[Tuple2[SoundSource, PatternElement]] = None

  def getFillers(): List[Playable] = {
    currentPlayables.map(p => new Playable(p.s, new AlgoPattern(this, p.s), Map()))
  }

  def getWeightParams(): List[Parameter]  = {
    currentPlayableInfo.flatMap(pi => {
      pi.weight match {
        case cv: ConstantValue => Some(Parameter.wrapExisting(cv, 0, 1))
        case _ => None
      }
    })
  }

  /**
   * get next trig for a certain soundsource
   * triggers the calculation for this step, if not done yet
   * @param counter
   * @param ss
   */
  def getNext(counter: Int, ss: SoundSource): PatternElement = {
    if (lastComputedStep != counter) {
      decidedTrig = decideNextNote(counter)
    }

    decidedTrig match {
      case Some((ss_, trig)) if (ss_ == ss) => trig
      case _ => PatternElement.x
    }
  }

  def decideNextNote(counter: Int): Option[Tuple2[SoundSource, PatternElement]] = {

    val sequencedNote = currentPlayables.exists(playable => {
      playable.p.getNext(counter) match {
        case PatternElement(Some(length), _, _) if length > 0 => true
        case _=>  false
      }
    })

    // if a note is already playing, don't add a filler note
    if (sequencedNote) {
      return None
    }

    // select which sound to play
    val epsilon = 0.01f // to avoid total weights of 0
    val weights = currentPlayableInfo.map(_.weight.getValue(0)+epsilon) // sid 0 means, we can't really modulate it
    val ran = Random.nextFloat() * weights.sum

    var cumul = 0f
    var index = 0

    while (ran >= cumul + weights(index)) {
      cumul = cumul + weights(index)
      index += 1
    }

    //val soundIndex = Random.nextInt(currentPlayables.length)
    val pInfo = currentPlayableInfo(index)

    // lower volume
    var trig = PatternElement.trig(1)
    pInfo.volume.foreach(vol =>
    trig = trig.lock(vol, vol.getNormalValue() * 0.5f))

    // add stutter
    val r = Random.nextFloat()
    if (r < 0.5) {

      val loopEnd = if (r < 0.1) 0.33f else if (r < 0.3) 0.25f else 0.5f
      //val loopEnd = 0.5f

      pInfo.stutEnable.foreach(se => trig = trig.lock((se, 1)))
      pInfo.stutLoopEnd.foreach(le => trig = trig.lock((le, loopEnd)))

    }

    val fillerSound = (pInfo.playable.s, trig)



    val reallyPlay = Random.nextFloat() <= fillProb.get()

    if (reallyPlay) return Some(fillerSound)

    return None

    // TODO: use the mods from the playable (and check both types of mods)
    // to make sure that we use pitch(es) that are already used in sequence

    // OR make a special trig that does not unlock the existing locks (prepared in PatternElement)



  }
}