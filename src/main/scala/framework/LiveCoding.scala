package framework


import framework.MyImplicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
// ms.p(rythmPattern = List(1)).cutoff(List(0.5)).frequency(List(100,200))

abstract class SoundSource {

  var triggerables: List[Env] = List()

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
  val filter = new SKF_OM_Diodes(osc * 5f, cutoff + env * 0.2f, 0.8f)

  val out: VCA = filter * env

  // add connect the audio output
  mixer.sources = out :: mixer.sources

  // make sure it can be triggered by the sequencer
  triggerables = env :: triggerables
}

//trait Pattern[Type] {
//  def getNext(): Type = 0
//}


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

}

class SequencerLane(ply: Playable) {
  var counter: Int = 0 // how many ticks have passed
  var playcounter: Int = 0 // incremented whenever a note is played

  var on = false

  def tick(): Unit = {
    val index = counter % ply.p.length

    if (on) {
      ply.s.release()
      on = false
    }

    if (ply.p(index) >= 1) {
      on = true
      ply.mods.foreach { tuple =>
        val modindex = playcounter % tuple._2.length
        tuple._1.value = tuple._2(modindex)
      }
      playcounter += 1
      ply.s.engage()
    }

    counter+=1

  }

}

class Sequencer(var bpm:Int = 120) {
  var lanes : List[SequencerLane] = List()
  var running = false

  def addLane(lane: SequencerLane): Unit = {
    lanes =   lane :: lanes
  }

  def start() = {
      running = true
      Future{run()}
  }

  def stop() = running = false

  def run(): Unit = {

    while(running) {
      lanes.foreach(_.tick())
      System.out.println("tick")

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
