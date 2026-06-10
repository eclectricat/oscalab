package live

import framework._
import framework.MyImplicits._



object Sounds {
  // initialise setup
  val mixer = new Mixer(List())
  val eng = new SoundEngine(mixer)
  eng.start()

  val s = new Sequencer(bpm=150)


  val snare = new SoundSource() {

    val dec = new ConstantValue(1f)
    val pitch = new ConstantValue(1f)
    val penv = new ExpEnv(0f, 0.1f * dec, 0f, 0f, None, 0)
    val aenv = new ExpEnv(0f, 0.2f * dec , 0f, 0f, None, 0)

    val o1 = new SinOsc(140f * pitch * new PitchToFreq(penv * 12))
    val o2 = new SinOsc(209f * pitch * new PitchToFreq(penv * 12))
    val tonal = new WaveShaper(0.0f + (o1 +  o2) * aenv * 2f, 1f)
    val noiseHP = new Digital2PoleHP(new NoiseOsc(), 0.2f, 0f) // new Digital2Pole(new NoiseOsc(), 0.8f, 0f)
    val noiseBP = new Digital2PoleBP(noiseHP, 0.4f, 0f)
    val nenv = new ExpEnv(0.001f, 0.4f* dec, 0f, 0f, None, 0)

    val noise = 2f * noiseBP * nenv
    val vol = new ConstantValue(1f)
    val stut = new Stutter((0.5f * tonal + 5f * noise)* vol, 0, 1, 0, 0, 150)

    mixer.sources = stut :: mixer.sources
    triggerables = triggerables ++ List(penv, aenv, nenv, stut)
  }

  val hh = new SoundSource() {
    val dec = new ConstantValue(1f)
    val vol = new ConstantValue(1f)
    val cut = new ConstantValue(0.6f)
    val env = new ExpEnv(0.001f, 0.1f* dec, 0f, 0f, None, 0)
    val noise = new Digital2PoleHP(new NoiseOsc(), cut, 0.4f) * env

    val stut = new Stutter(noise * vol, 0, 1, 0, 0, 150)

    mixer.sources = stut :: mixer.sources
    triggerables = triggerables ++ List(env, stut)
  }

  val kick = new SoundSource() {

    val odf = new ConstantValue(1f)
    val vol = new ConstantValue(1f)
    val dec = new ConstantValue(1f)
    val env1 = new ExpEnv(0f, .2f, 0f, 0f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
    val env2 = new ExpEnv(0f, 0.5f * dec, 0f, 0.1f * dec, None, 0) // new ExpEnv(0f, 0.2f, 0f, 0f, None, 0)
    //val o1 = new SinOsc(50f + env1 * 150f, 1.5f) * env2 * 1f
    val freq = new PitchToFreq(env1 * 24f) *  50f
    val o1 = new SinOsc(freq) * env2
    val od = new WaveShaper(o1, 3f * odf) * vol
    val stut = new Stutter(od, 0f, 1f, 1f, 0.0f, 150)
    mixer.sources = stut :: mixer.sources
    triggerables = triggerables ++ List(env1, env2, stut)
  }

  //mixer.sources = List()

  s.set(1,
    kick.p(List(1, 0, 1, 0, 0, 0, 0, 0)++ List(0,0,0,0,0,0,0,0) ++ List(1, 1, 0, 0, 0, 0, 1, 0)++ List(0,0,0,0,0,0,0,0))
      .mod(kick.vol -> (List(1, 0.2f) ++ List(0.7f,0.85f,1)))
      .mod(kick.odf -> (List(1, 3f) ++ List(3f,1,1)))
      .mod(kick.dec -> (List(1, 7f) ++ List(1f,4,1)))
      .modMaybe(kick.stut.enable -> List(0,1f,0,0,0f))

  )

  s.set(2,
    snare.p(List(0,0,0,0,0,0, 0,0)++List(1,0,0,1,0,1,0,1))
      .mod(snare.dec -> List(1, 0.5f, 1, 2f , 0.7f, 0.8f))
      .mod(snare.vol -> List(1, 0.3f, 0.2f, 0.2f ))
      .mod(snare.pitch -> List(1, 2f, 1.8f, 1f))
      .modMaybe(snare.stut.enable -> List(0f,0,0,1))
      .modMaybe(snare.stut.loopEnd -> List.fill(4 * 3)(0.1f).updated(3, 1f).updated(7, 0.2f))
  )

  s.set(3,
    hh.p(List(1,0,1,1,   0,0,0,0,  1,1,0,1, 0,0,1,1,  0)).mod(hh.dec -> List(0.5f, 0.4f, 1f, 0.5f, 2.5f, 1f, 1f))
      .modMaybe(hh.stut.enable -> List(0, 1, 0f, 0f, 0f, 1f))
      .modMaybe(hh.stut.loopEnd -> List(0.5f, 0.2f, 0.75f))
      .mod(hh.cut -> List(0.6f, 0.4f, 0.8f, 0.7f, 0.5f))

  )

  s.set(3,
    hh.p(List(1,0,1,1,   0,0,0,0,  1,0,0,0, 0,0,1,1,  0)).mod(hh.dec -> List(0.5f, 0.4f, 1f, 0.5f, 2.5f, 1f, 1f))
      .modMaybe(hh.stut.enable -> List(0, 1, 0f, 0f, 0f, 1f, 0f))
      .modMaybe(hh.stut.loopEnd -> List(0.5f, 0.2f, 0.75f))
      .mod(hh.cut -> List(0.6f, 0.4f, 0.8f, 0.7f, 0.5f))

  )

  hh.vol.value = 2f

  //snare.stut.loopEnd.asInstanceOf[ConstantValue].value = 1f
  //snare.engage()
  //snare.release()

  //kick.stut.move.asInstanceOf[ConstantValue].value = 0f
  //kick.stut.loopEnd.asInstanceOf[ConstantValue].value = 0.2f
  //kick.stut.loopStart.asInstanceOf[ConstantValue].value = 0f
  //kick.engage()
  //kick.release()
  s.start()
  //s.stop()
  //s.bpm=155
}
