
import framework._
import framework.MyImplicits._
import framework.LiveImplicits._

import scala.util.Random
import scala.language.reflectiveCalls


// initialise setup
val mixer = new Mixer(List())
val eng = new SoundEngine(mixer)
eng.start()
val s = new Sequencer(bpm = 160)


val snare = new SoundSource() {

  val prob_ = new Parameter(1f, 0, 1, "SnareProb")

  override def prob() = prob_.get()

  val dec = new ConstantValue(1f)
  val pitch = new ConstantValue(1.5f)
  val penv = new ExpEnv(0f, 0.1f * dec, 0f, 0f, None, 0)
  val aenv = new ExpEnv(0f, 0.2f * dec, 0f, 0f, None, 0)

  val o1 = new SinOsc(140f * pitch * new PitchToFreq(penv * 8))
  val o2 = new SinOsc(209f * pitch * new PitchToFreq(penv * 8))
  val tonal = new WaveShaper(0.0f + (o1 + o2) * aenv * 2f, 1f)
  val noiseHP = new Digital2PoleHP(new NoiseOsc(), 0.2f, 0f) // new Digital2Pole(new NoiseOsc(), 0.8f, 0f)
  val noiseBP = new Digital2PoleBP(noiseHP, 0.5f, 0.7f)
  val nenv = new ExpEnv(0.001f, 0.4f * dec, 0f, 0f, None, 0)

  val noise = 2f * noiseBP * nenv
  val vol = new ConstantValue(0.2f)
  val stut = new Stutter((0.5f * tonal + 4f * noise) * vol, 0, 1, 0, 0, 150)
  val reduction = new Parameter(0f, 0f, 1f, "SRR")
  val srr = new RandomRate(stut, reduction)
  mixer.sources = srr :: mixer.sources
  triggerables = triggerables ++ List(penv, aenv, nenv, stut)
}

val hh = new SoundSource() {
  val dec = new ConstantValue(1f)
  val vol = new ConstantValue(1f)
  val cut = new ConstantValue(0.6f)
  val env = new ExpEnv(0.001f, 0.1f * dec, 0f, 0f, None, 0)
  val noise = new Digital2PoleHP(new NoiseOsc(), cut, 0.4f) * env

  val stut = new Stutter(noise * vol, 0, 1, 0, 0, 150)
  val reduction = new Parameter(0.2f, 0f, 1f, "SRR")
  //val srr = new RandomRate(stut, reduction)

  mixer.sources = stut :: mixer.sources
  triggerables = triggerables ++ List(env, stut)

  val prob_ = new Parameter(1f, 0, 1, "HHProb")

  override def prob() = prob_.get()
}

val kick = new SoundSource() {

  val odf = new ConstantValue(2f)
  val vol = new ConstantValue(0.2f)
  val dec = new ConstantValue(1f)
  val freqMul = new ConstantValue(1f)
  val env1 = new ExpEnv(0f, .2f, 0f, 0f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
  val env2 = new ExpEnv(0f, 0.5f * dec, 0f, 0.1f * dec, None, 0) // new ExpEnv(0f, 0.2f, 0f, 0f, None, 0)
  //val o1 = new SinOsc(50f + env1 * 150f, 1.5f) * env2 * 1f
  val freq = new PitchToFreq(env1 * 24f) * 50f * freqMul
  val o1 = new SinOsc(freq) * env2
  val od = new WaveShaper(o1, 5f * odf) * vol
  val stut = new Stutter(od, 0f, 1f, 0f, 0.0f, 150)
  mixer.sources = stut :: mixer.sources
  triggerables = triggerables ++ List(env1, env2, stut)

  val prob_ = new Parameter(1f, 0, 1, "BDProb")

  override def prob() = prob_.get()
}

/*val synth = new SoundSource() {

  var voiceIndex: Int = 0

  val pitch = new ConstantValue(0f) // frequency that is applied at the note trigger time
  val cutoff = new Parameter(0.5f, 0, 1f, "SynthCut")
  val voices: Seq[(ExpEnv, ConstantValue)] = List(1, 2, 3, 4, 5).map(_ => newVoice())

  def newVoice(): (ExpEnv, ConstantValue) = {
    val env1 = new ExpEnv(0f, 8f, 0f, 8f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
    val discenv = new Discretizr(env1, 6)
    val pitchCopy = new ConstantValue(this.pitch.value)
    val freq = new PitchToFreq(pitchCopy) * 110f
    val lfo = new SinOsc(1f)
    val o1 = new SawOscB(freq + 2f * lfo) //new SawOscB(freq+2*lfo)
    val output = new Digital2Pole(o1, cutoff, 0.5f) * discenv * 0.2f
    val pan = new PanBalance(output, (Random.nextFloat() * 2 - 1))
    mixer.sources = pan :: mixer.sources
    (env1, pitchCopy)
  }

  override def engage() = {
    voiceIndex = (voiceIndex + 1) % voices.length
    voices(voiceIndex) match {
      case (env: Triggerable, vpitch: ConstantValue) => {
        vpitch.value = pitch.value
        env.retrigger()
      }
    }
  }

  override def release(): Unit = {
    voices(voiceIndex)._1.release()
  }
}

*/


s.set(1,
  kick.p(List(1, 0, 0, 0, 0, 0, 0, 0, 0,0,1,0,0,0,1,0) ++
    List(1, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,1))
)


s.set(2,
  snare.p(ListPattern(0,0,0,0,0,0,0,0, 1, 0,0,0,0,0,0,0))
    .mod(snare.pitch -> Locks(1, 2f, 1.8f, 1f))
)

s.set(3,
  hh.p(List(0,0,1,0,0,0,1,0) ++ List(1,0,0,0,1,0,1,1) )
    .mod(hh.dec -> Locks(0.5f, 0.4f, 1f, 0.5f, 2.5f, 1f, 1f))
)


//val sequencedPlayables = List(1,2,3).flatMap(s.lanes.get(_)).map(_.ply)

val kickVolParam = Parameter.wrapExisting(kick.vol, 0, 1)


val sequencedPlayables = List(
  new PlayableInfo(s.lanes(1).ply, stutter = Some(kick.stut), volume = Some(kickVolParam)),
  new PlayableInfo(s.lanes(2).ply, stutter = Some(snare.stut), volume = Some(Parameter.wrapExisting(snare.vol, 0, 1))),
  new PlayableInfo(s.lanes(3).ply, stutter = Some(hh.stut), volume = Some(Parameter.wrapExisting(hh.vol, 0, 1)))
)

val fillAlgo = new FillAlgo(sequencedPlayables)
val fillerPlayables = fillAlgo.getFillers()


fillerPlayables.map(playable => {
  val index = fillerPlayables.indexOf(playable)
  s.set("fill"+(index+1), playable)
}
)

// put additional mods to the filler lane: vol and pitch
s.set("fill1", s.lanes("fill1").ply.mod2(kick.freqMul -> List(2f)).mod(kickVolParam -> List(0.1f)))


//s.solo(List(4,5))
s.start()

val weightAdapter = fillAlgo.getWeightParams().map(param => new ParameterSliderAdapter(param))

new SliderPanel(weightAdapter ++ List(
  new ParameterSliderAdapter(fillAlgo.fillProb),
  new ParameterSliderAdapter(snare.prob_),
  new ParameterSliderAdapter(kick.prob_),
  new ParameterSliderAdapter(hh.prob_)
)).show()


/*new SliderPanel(List(
  new ParameterSliderAdapter(snare.prob_),
  new ParameterSliderAdapter(kick.prob_),
  new ParameterSliderAdapter(hh.prob_),
  new ParameterSliderAdapter(Parameter.wrapExisting(fm.modInt, -10, 10)),
  new ParameterSliderAdapter(synth.cutoff))).show()
 */

