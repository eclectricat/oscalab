import javax.sound.midi.MidiSystem
import javax.sound.midi.Receiver
import javax.sound.midi.Transmitter
import javax.sound.midi._

import MyImplicits._

class MonoSynth(deviceId:String = "") extends MyReceiver(deviceId) with StandardKnobs {

  val freq = new ConstantValue(100f)
  val r = scala.util.Random
  var (sound,envs)  = newVoice(freq)
  envs.map(_.state=0)

  var lastPressedNote = -1

  val mx = new Mixer(List(sound))
  val eng = new SoundEngine(mx)
  eng.start()

  showUI()

  override def close(): Unit = {super.close(); eng.stop()}

  override def send(msg: javax.sound.midi.MidiMessage,value: Long): Unit = {

    msg match {
      case a: ShortMessage => {

        a.getCommand match {

          case ShortMessage.NOTE_ON => {
            System.out.println("NoteOn: ")
            System.out.println("Data: " + a.getData1())
            System.out.println("Data: " + a.getData2())
            val note = a.getData1()

            lastPressedNote = note

            val f =  (440f * math.pow(2, ((note)-69)/12f)).toFloat
            freq.value = f

            envs.map(_.retrigger())

          }

          case ShortMessage.NOTE_OFF => {
            System.out.println("NoteOff: ")
            System.out.println("Data: " + a.getData1())
            System.out.println("Data: " + a.getData2())
            val note = a.getData1()

            if (note == lastPressedNote) {
              envs.map(_.release())
            } else {
              System.out.println("it seems that this note is not playing anymore, do nothing")
            }

          }
        }
      }

      case _ => None
    }
  }

  def newVoice(freq: SiGen, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    //val freq =  (440f * math.pow(2, ((note+detune)-69)/12f)).toFloat

    val sound1 = new SawOscB(freq, initialPhase=r.nextFloat()) //* 0.99f

    //val freqMod = new SinOsc(1) *  (freq * 0.005f)
    //val totalFreq = freqMod + (freq + freq*0.01f)
    val totalFreq = (freq + 0.5f)
    val sound2 = new SawOscB(totalFreq, initialPhase=r.nextFloat()) //* 0.99f

    var totalSound: SiGen = sound1 + sound2 * osc2level // * -1
    val env = new Env(attack.getValue(0), 0.5f, 1f, release.getValue(0), None, 0)
    val filterEnv = new Env(attack.getValue(0), release.getValue(0), 0.3f, release.getValue(0), None, 0)
    totalSound = new Digital2Pole(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    totalSound = totalSound *  env
    //totalSound = new WaveFolder(totalSound, 1f, 3)
    return (totalSound, List(env, filterEnv))
  }


}
