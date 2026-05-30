import framework.MyImplicits._
import scala.collection.immutable.List


// initialise setup
val mixer = new Mixer(List())
val eng = new SoundEngine(mixer)
eng.start()

val s = new Sequencer(bpm = 120)


val sound = new Sound(mixer)

// TODO, add nicer syntax for this
/*val playable = new Playable(sound, List(1,0,1),
  Map(
    sound.freq -> List(110f, 220f),
    sound.cutoff -> List(0.2f, 0.2f, 0.4f, 0.6f, 0.5f)
  ))
 */


//s.addLane(new SequencerLane(playable))



val snare = new SoundSource() {

  val env1 = new ExpEnv(0f, 0.05f, 0f, 0f, None, 0)
  val env2 = new ExpEnv(0f, 0.2f, 0f, 0f, None, 0)
  val env3 = new ExpEnv(0f, 0.1f, 0f, 0f, None, 0)
  val env4 = new ExpEnv(0f, 0.1f, 0f, 0f, None, 0)
  val o1 = new SinOsc(
    140f + env1 * 150f, 1.5f
  ) * env2 * 1f
  val o2 = new SinOsc(
    240f + env3 * 150f, 1.5f
  ) * env4 * 1f

  val tonal = new WaveShaper(0.0f + (o1+o2), 5f)

  val noiseLP = new Digital2Pole(new NoiseOsc(), 0.8f, 0f)
  val noiseBP = new Digital2PoleHP(noiseLP, 0.5f, 0f)

  val env5 = new ExpEnv(0.001f, 0.1f, 0f, 0f, None, 0)
  val env6 = new ExpEnv(0.001f, 0.3f, 0f, 0f, None, 0)
  val noise = 0.5f * noiseLP * env5 + 1f * noiseBP * env6

  mixer.sources = (0.5f * tonal + 5f * noise) :: mixer.sources
  triggerables = triggerables ++ List(env1, env2, env3, env4, env5, env6)
}

s.addLane(new SequencerLane(
  sound.p(List(1, 0, 1))
    .mod(sound.freq -> List(110f, 220f))
    .mod(sound.cutoff -> List(0.2f, 0.2f, 0.4f, 0.6f, 0.5f))
)
)

s.addLane(new SequencerLane(
  snare.p(List(1,1,1,0,0,0,0,1,0))
))
