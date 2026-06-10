package framework


import framework.MyImplicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
// ms.p(rythmPattern = List(1)).cutoff(List(0.5)).frequency(List(100,200))

abstract class SoundSource {

  var triggerables: List[Triggerable] = List()

  def engage() = triggerables.foreach(_.retrigger())
  def release() = triggerables.foreach(_.release())

  // efficient syntax to create a playable
  def p(pattern: List[Int]): Playable =
    new Playable(this, pattern, Map())

}

// make an explicit class here, but also try with anonymous classes later
class Sound(mixer: Mixer) extends SoundSource {

  val freq = new ConstantValue(110f)
  val cutoff = new ConstantValue(0.5f)
  val osc = new SawOscB(freq)
  val env = new ExpEnv(0f, 1f, 0f, 1f, None, 0)
  val filter = new SKF_OM_Diodes(osc * 1f, cutoff + env * 0.2f, 0.8f)

  val out: VCA = filter * env
  val out2 = new WaveShaper(out, 5f) * 0.3f

  // add connect the audio output
  mixer.sources = out2 :: mixer.sources

  // make sure it can be triggered by the sequencer
  triggerables = env :: triggerables
}

case class PatternElement(
                           trigLength: Option[Int] = None,
                           locks: Map[ConstantValue, Float] = Map.empty,
                           prob: Float = 1,
                         ) {
  def %(p: Float): PatternElement = {
    this.copy(prob = p)
  }

  def lock(kv: (ConstantValue, Float)): PatternElement = {
    copy(locks = locks + kv)
  }

}

object PatternElement extends  NestablePatternElement {
  def x = new PatternElement()
  def trig(length: Int) = new PatternElement(trigLength = Some(length))
}




// Example of patternelements
/*
- a trig with a length
- no trig
- a trig with multiple param locks
- only param locks
- probabilty of trig
- probability of param lock

 */

trait Pattern extends NestablePatternElement {
  def getNext(): PatternElement
}

// just to make it nestable
trait NestablePatternElement

case class ListPattern(var elements: List[NestablePatternElement]) {
  var counter = 0

  def getNext() : PatternElement = {
    val index = counter % elements.length
    counter += 1
    elements(index) match {
      case pattern: Pattern =>
        return PatternElement.x // TODO: handle the nested case _: framework.Pattern => ???
      case p: PatternElement => return p
      //case _ => ???
    }
  }
}


/**
 * All the information a sequencer needs to play
 */
class Playable(val s: SoundSource,
               val p: List[Int],
               var mods: Map[ConstantValue, List[Float]])  {

  // nice syntax to add parameter automation
  def mod(pair: (ConstantValue, List[Float])) = {
    mods += pair
    this
  }

  /**
   * Check if it is a ConstantValue, and if yes, modulate it
   * @param pair of signal to modulate and Sequence of values
   * @return this
   */
  def modMaybe(pair: (SiGen, List[Float])): Playable = {
    pair._1 match {
      case value: ConstantValue =>
        mod((value, pair._2))
      case _ => this
    }
  }

}

class SequencerLane(var ply: Playable) {
  var counter: Int = 0 // how many ticks have passed
  var playcounter: Int = 0 // incremented whenever a note is played

  var muted = false
  var noteOn = false

  def tick(): Unit = {
    val index = counter % ply.p.length

    if (noteOn) {
      ply.s.release()
      noteOn = false
    }

    if (ply.p(index) >= 1)  {
      if (!muted) {
        noteOn = true
        ply.mods.foreach { tuple =>
          val modindex = playcounter % tuple._2.length
          tuple._1.value = tuple._2(modindex)
        }
        ply.s.engage()
      }
      playcounter += 1 // increase playcounter also when muted, so it stays in sync
    }

    counter+=1

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

  def addLane(laneId: Any, lane: SequencerLane): Unit = {
    lanes =   lanes + (laneId -> lane)
  }

  def start() = {
      running = true

      lanes.values.foreach(_.reset())

      Future{run()}
  }

  def stop() = running = false

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
