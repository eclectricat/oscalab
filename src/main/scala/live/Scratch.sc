

import framework._
import framework.MyImplicits._
import framework.LiveImplicits._

  // initialise setup
  val mixer = new Mixer(List())
  val eng = new SoundEngine(mixer)
  eng.start()

  val s = new Sequencer(bpm=120)


  // TODO, add nicer syntax for this
  /*val playable = new Playable(sound, List(1,0,1),
  Map(
    sound.freq -> List(110f, 220f),
    sound.cutoff -> List(0.2f, 0.2f, 0.4f, 0.6f, 0.5f)
  ))
 */
  //s.addLane(new SequencerLane(playable))

  val sound = new Sound(mixer) // a synth sound

  val snare = new SoundSource() {

    val env1 = new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
    val env2 = new ExpEnv(0f, 0.2f, 0f, 0f, None, 0)
    val env3 = new ExpEnv(0f, 0.1f, 0f, 0f, None, 0)
    val env4 = new ExpEnv(0f, 0.1f, 0f, 0f, None, 0)
    val o1 = new SinOsc(140f + env1 * 150f, 1.5f) * env2 * 1f
    val o2 = new SinOsc(240f + env3 * 150f, 1.5f) * env4 * 1f

    val tonal = new WaveShaper(0.0f + (o1 + o2), 5f)

    val noiseLP = new Digital2Pole(new NoiseOsc(), 0.8f, 0f)
    val noiseBP = new Digital2PoleHP(noiseLP, 0.5f, 0f)

    val env5 = new ExpEnv(0.001f, 0.1f, 0f, 0f, None, 0)
    val env6 = new ExpEnv(0.001f, 0.3f, 0f, 0f, None, 0)
    val noise = 0.5f * noiseLP * env5 + 1f * noiseBP * env6

    mixer.sources = (0.5f * tonal + 5f * noise) :: mixer.sources
    triggerables = triggerables ++ List(env1, env2, env3, env4, env5, env6)
  }

  val kick = new SoundSource() {
    val env1 = new ExpEnv(0f, .2f, 0f, 0f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
    val env2 = new ExpEnv(0f, 0.5f, 0f, 0.1f, None, 0) // new ExpEnv(0f, 0.2f, 0f, 0f, None, 0)
    //val o1 = new SinOsc(50f + env1 * 150f, 1.5f) * env2 * 1f
    val freq = new PitchToFreq(env1 * 24f) *  50f
    val o1 = new SinOsc(freq) * env2 * 1f
    val od = new WaveShaper(o1, 3f)
    mixer.sources = od :: mixer.sources
    triggerables = triggerables ++ List(env1, env2)
  }

  val synth = new SoundSource() {

    var voiceIndex: Int = 0

    val pitch = new ConstantValue(0f) // frequency that is applied at the note trigger time
    val cutoff = new ConstantValue(0.5f)
    val voices: Seq[(ExpEnv, ConstantValue)] = List(1,2,3).map(_ => newVoice())

    def newVoice(): (ExpEnv, ConstantValue) = {
      val env1 = new ExpEnv(0f, 1.5f, 0f, 1.5f, None, 0) //new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
      val pitchCopy = new ConstantValue(this.pitch.value)
      //val pitchCopy = new ConstantValue(0f)
      val freq = new PitchToFreq(pitchCopy) * 220f
      val o1 = new SawOscB(freq)
      val output = new Digital4PoleFP(o1, cutoff, 0.5f) * env1
      mixer.sources = output :: mixer.sources
      return (env1, pitchCopy)
    }

    override def engage() = {
      voiceIndex = (voiceIndex + 1) % voices.length
      voices(voiceIndex) match {
        case (env:Triggerable, vpitch:ConstantValue) => {
          vpitch.value = pitch.value
          env.retrigger()
        }
      }
      //voices(voiceIndex).foreach(_.retrigger())
      System.out.println(voiceIndex)
    }

    override def release(): Unit = {
      voices(voiceIndex)._1.release()
    }

  }

  //mixer.sources = List()

  s.addLane(1, new SequencerLane(
    sound.p(List(1, 0, 1, 0, 0, 1))
      .mod(Parameter.wrapExisting(sound.freq) -> List(110f, 220f))
      .mod(sound.cutoff -> List(0.2f, NoLock, 0.4f, 0.1f, 0.5f, NoLock,NoLock))
  ))

  s.addLane(2, new SequencerLane(
    snare.p(List(0,0,0,0,1,0, 0,0))
      .modMaybe(snare.env6.dec -> List(0.1f, 0.1f, 0.2f, 0.25f, 0.05f, 0.4f))
      .modMaybe(snare.env5.dec -> List(0.6f, 0.1f, 0.2f, 0.25f, 0.05f ))
  ))

  /*s.addLane(1, new SequencerLane(
    snare.p(List(0))
  ))*/

  s.addLane(3, new SequencerLane(
    kick.p(ListPattern(1, 0, 0, 0, 1, 0, 0, 1%%0.4f))
  ))

  s.addLane(4, new SequencerLane(
    synth.p(ListPattern(1,0,0)).mod(Parameter.wrapExisting(synth.pitch) -> List(0f,12f,5f,12f,6f))
  ))

  s.mute(1)
  s.mute(2)
  s.mute(3)

  s.start()
  //s.stop()

  new SliderPanel(List(
    new ParameterSliderAdapter(sound.prob_),
    new ParameterSliderAdapter(sound.cutoff),
    new ParameterSliderAdapter(sound.reso))).show()


