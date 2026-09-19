
import framework._
import framework.MyImplicits._
import framework.LiveImplicits._

import scala.util.Random
import scala.language.reflectiveCalls


// initialise setup
val mixer = new Mixer(List())
//val eng = new SoundEngine16(mixer)
val eng = new SoundEngine(mixer)
eng.start()
val s = new Sequencer(bpm = 150)


val snare = new SoundSource() {

  val prob_ = new Parameter(1f, 0, 1, "SnareProb")

  override def prob() = prob_.get()

  val dec = new ConstantValue(1f)
  val pitch = new ConstantValue(0.75f)
  val penv = new ExpEnv(0f, 0.1f * dec, 0f,0.1f * dec, None, 0)
  val aenv = new ExpEnv(0f, 0.4f * dec, 0f, 0.4f * dec, None, 0)

  val o1 = new SinOsc(140f * pitch * new PitchToFreq(penv * 12))
  val o2 = new SinOsc(209f * pitch * new PitchToFreq(penv * 12))
  val tonal = new WaveShaper(0.0f + (o1 + o2) * aenv * 2f, 1f)
  val noiseHP = new Digital2PoleHP(new NoiseOsc(), 0.2f, 0f) // new Digital2Pole(new NoiseOsc(), 0.8f, 0f)
  val noiseBP = new Digital2PoleBP(noiseHP, 0.4f, 0f)
  val nenv = new ExpEnv(0.001f, 0.6f * dec, 0f, 0.6f * dec, None, 0)
  val henv = new ExpEnv(0.1f, 1f * dec, 0f, 1f * dec, None, 0)

  val noise = 2f * noiseBP * nenv + 0.2f  *noiseHP * henv
  val vol = new ConstantValue(0.5f)
  val stut = new Stutter((1.5f * tonal + 5f * noise) * vol, 0, 1, 0, 0, 150)
  val decimate = new Parameter(0f ,0f,1f, "decimate")
  val rate = new RandomRate(stut, decimate.signal)
  val route = rate // new ChannelShifter(stut, List(0), List(3))
  mixer.sources = route :: mixer.sources
  //mixer.sources = stut :: mixer.sources
  triggerables = triggerables ++ List(penv, aenv, nenv, stut)
}

val hh = new SoundSource() {
  val dec = new ConstantValue(1f)
  val vol = new ConstantValue(1f)
  val cut = new ConstantValue(0.6f)
  val env = new ExpEnv(0.001f, 0.1f * dec, 0f, 0f, None, 0)
  val noise = new Digital2PoleHP(new NoiseOsc(), cut, 0.4f) * env

  val stut = new Stutter(noise * vol, 0, 1, 0, 0, 150)
  val route = stut // new ChannelShifter(stut, List(0), List(4))
  mixer.sources = route :: mixer.sources
  triggerables = triggerables ++ List(env, stut)

  val prob_ = new Parameter(1f, 0, 1, "HHProb")

  override def prob() = prob_.get()
}

val kick = new SoundSource() {

  val odf = new ConstantValue(1f)
  val vol = new ConstantValue(1f)
  val dec = new ConstantValue(9f)
  val env1 = new ExpEnv(0f, .2f, 0f, 0f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
  val env2 = new ExpEnv(0f, 0.5f * dec, 0f, 0.1f * dec, None, 0) // new ExpEnv(0f, 0.2f, 0f, 0f, None, 0)
  //val o1 = new SinOsc(50f + env1 * 150f, 1.5f) * env2 * 1f
  val freq = new PitchToFreq(env1 * 24f) * 50f
  val o1 = new SinOsc(freq) * env2
  //val o1 = new PulseOsc(freq) * env2
  val od = new WaveShaper(o1, 10f * odf) * vol
  val stut = new Stutter(od, 0f, 1f, 0f, 0.0f, 150)
  val route = stut // new ChannelShifter(stut, List(0), List(2))

  mixer.sources = route :: mixer.sources
  triggerables = triggerables ++ List(env1, env2, stut)

  val prob_ = new Parameter(1f, 0, 1, "BDProb")

  override def prob() = prob_.get()
}

val synth = new SoundSource() {

  var voiceIndex: Int = 0

  val pitch = new ConstantValue(0f) // frequency that is applied at the note trigger time
  val cutoff = new Parameter(0.5f, 0, 1f, "SynthCut")
  val voices: Seq[(ExpEnv, ConstantValue)] = List(1, 2, 3, 4, 5).map(_ => newVoice())

  def newVoice(): (ExpEnv, ConstantValue) = {
    val env1 = new ExpEnv(0f, 8f, 0f, 8f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
    val discenv = env1 //new Discretizr(env1, 6)
    val pitchCopy = new ConstantValue(this.pitch.value)
    val freq = new PitchToFreq(pitchCopy) * 100f
    val lfo = new SinOsc(1f)
    val o1 = new SawOscB(freq + 0f * lfo) //new SawOscB(freq+2*lfo)
    val o2 = new SawOscB(freq * 1.5f + 0f * lfo) //new SawOscB(freq+2*lfo)
    val output = new Digital2Pole(o1 + 1f * o2, cutoff, 0.5f) * discenv * 0.2f
    val red = new RandomRate(output, 0.92f)
    val pan = new PanBalance(red, (Random.nextFloat() * 2 - 1))
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

val fm = new SoundSource() {
  val pitch = new ConstantValue(0)
  val freq = new PitchToFreq(pitch) * 50f
  val mod1 = new SinOsc(freq * 4)
  val lfo = new SinOsc(1.5f)
  val modenv = new ExpEnv(0f, 0.5f, 1f , 0.5f, None, 0)
  val modInt = new ConstantValue(2.5f)
  val car1 = new SinOsc(freq, phaseOffset = mod1 * modInt * modenv *  lfo )
  val env1 = new ExpEnv(0.001f,3f,0,3f, None, 0)
  val route = new ChannelShifter(env1 * car1, List(0,1), List(0,1))
  mixer.sources = route :: mixer.sources
  triggerables = env1 :: triggerables
}

/*val drone = new SoundSource() {
  val freq1 = new SinOsc(0.3f) * 3f + 55f
  val freq2 = new SinOsc(0.2f) * 2f + 110f
  val mod1int = new SinOsc(0.1f) * 2f
  val mod2int = new SinOsc(0.13f) * 1.5f
  val mod1 = new SinOsc(freq1)
  val car1 = new SinOsc(freq2, phaseOffset = mod1 * (mod1int + 1.5f))
  val mod2 = new SinOsc(freq2 * 1.02f)
  val car2 = new SinOsc(freq1 * 0.99f, phaseOffset = mod2 * (mod2int + 2f))

  val env = new ExpEnv(1f, 1f, 1f, 3f, None, 0)

  val l = new PanBalance(car1 * env * 0.1f, -1f)
  val r = new PanBalance(car2 * env * 0.1f, 1f)

  mixer.sources = (l + r) :: mixer.sources
  triggerables = env :: triggerables
}*/


s.set(1,
  kick.p(List(1, 0, 1, 0, 0, 0, 0, 0) ++ List(0, 0, 0, 0, 0, 0, 1, 0) ++ List(0, 0, 0, 0, 1, 0, 0, 0) ++ List(0, 0, 0, 0, 0, 0, 0, 0))
    /*.mod(kick.vol -> (Locks(1, 0.2f) ++ Locks(0.7f, 0.85f, 1)))
    .mod(kick.odf -> (Locks(1, 3f) ++ Locks(3f, 1, 1)))
    .mod(kick.dec -> (Locks(1, 7f) ++ Locks(1f, 4, 1)))
    .modMaybe(kick.stut.enable -> Locks(0, 1f, 0, 0, 0f))*/

)

//import ListPattern._

s.set(2,
  snare.p(ListPattern(
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0 ,
    0, 0, 0, 0, 0, 0, 0, 0,
    1 %% 0.5f, 0, 1, 0, 0, 0, 0, 0))
    .mod(snare.pitch -> Locks(NoLock, 2f, NoLock))
    .mod(snare.vol -> Locks(NoLock, 0.1f, NoLock))
   /* .mod(snare.dec -> Locks(FloatLock(1), 0.5f, 1, 2f, 0.7f, 0.8f))
    .mod(snare.vol -> Locks(0.5f, 0.3f, 0.2f, 0.2f))
    .mod(snare.pitch -> Locks(1, 2f, 1.8f, 1f))
    .modMaybe(snare.stut.enable -> Locks(0f, NoLock, 0, 1))
    .modMaybe(snare.stut.loopEnd -> List.fill(4 * 3)(0.1f).updated(3, 1f).updated(7, 0.2f).map(FloatLock(_)))

    */
)

s.set(3,
  hh.p(List(
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0))
    /*
    .mod(hh.dec -> Locks(0.5f, 0.4f, 1f, 0.5f, 2.5f, 1f, 1f))
    .modMaybe(hh.stut.enable -> Locks(0, 1, 0f, 0f, 0f, 1f))
    .modMaybe(hh.stut.loopEnd -> List(0.5f, 0.2f, 0.75f))
    .mod(hh.cut -> Locks(0.6f, 0.4f, 0.8f, 0.7f, 0.5f))*/

)

s.set(3,
  hh.p(List(1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0))
    .mod(hh.dec -> Locks(0.5f, 0.4f, 1f, 0.5f, 2.5f, 1f, 1f))
    .modMaybe(hh.stut.enable -> List(0, 1, 0f, 0f, 0f, 1f, 0f))
    .modMaybe(hh.stut.loopEnd -> List(0.5f, 0.2f, 0.75f))
    .mod(hh.cut -> Locks(0.6f, 0.4f, 0.8f, 0.7f, 0.5f))

)

val scale = Vector(0, 2, 3, 5, 7, 8, 10, 12, 14, 15, 17, 18, 20, 22)
val n = 32
val notes = Iterator.iterate(0)(i => (i * 3 + 1) % scale.size).take(n).map(scale).toList

val transposed = notes.map(note => note + 5)
val transposed2 = transposed.map(note => note - 12)
val locks: List[FloatLock] = notes.map(f => floatToFloatLock(f)).toList
val locksT: List[FloatLock] = transposed.map(f => floatToFloatLock(f)).toList
val locksT2: List[FloatLock] = transposed2.map(f => floatToFloatLock(f)).toList
s.set(4,
  synth.p(ListPattern(1, 0, 0, 0,    0, 0, 1, 0,   0,  0, 0, 0,    0, 0, 1, 0))
    .mod(Parameter.wrapExisting(synth.pitch) -> locks)
    .mod(synth.cutoff -> Locks(NoLock, 0.1f, NoLock, 0.2f, NoLock))
)

/*s.set(4,
  synth.p(ListPattern(1,0,0,0,  0,0,0,0, 0,0,0,0,  1,0,0,0))
    .mod(Parameter.wrapExisting(synth.pitch) -> locksT2)
    .mod(synth.cutoff -> Locks(NoLock, 0.2f, NoLock, 0.5f, NoLock))
)*/

/*s.set(5,
  fm.p(ListPattern(1, 0 ,1,1, 0, 1, 0, 0, 1, 0, 0, 0 )).mod(fm.modInt -> Locks(0.5f,1, NoLock, 1.5f, 0.7f, NoLock, NoLock))
)*/

s.set(5,
  fm.p(ListPattern(1, 0 ,0, 0, 0,0,0,0 ,  0, 0, 0,0, 1, 0,0,0,    0, 0, 0, 0,  0,0,0,0,  1, 0,0,0,0,0,0,0 ))
    .mod(fm.modInt -> Locks(0.5f,1, NoLock, 1.5f, 0.7f, NoLock, NoLock))
    .mod(fm.pitch -> Locks(0, 7, -2))
)

hh.vol.value = 2f
hh.stut.enable.asInstanceOf[ConstantValue].value = 0


s.solo(List(1,2,4, 5))
s.start()
new SliderPanel(List(
  new ParameterSliderAdapter(snare.prob_),
  new ParameterSliderAdapter(kick.prob_),
  new ParameterSliderAdapter(hh.prob_),
  new ParameterSliderAdapter(Parameter.wrapExisting(fm.modInt, -10, 10)),
  new ParameterSliderAdapter(synth.cutoff))).show()

new SliderPanel(List(
  new ParameterSliderAdapter(Parameter.wrapExisting(snare.dec, 0,10)),
  new ParameterSliderAdapter(Parameter.wrapExisting(kick.dec, 0,10)),
  new ParameterSliderAdapter(snare.decimate)

)
).show()
//s.stop()
//s.bpm=155

