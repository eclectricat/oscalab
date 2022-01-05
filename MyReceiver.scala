import javax.sound.midi.MidiSystem
import javax.sound.midi.Receiver
import javax.sound.midi.Transmitter
import javax.sound.midi._

import MyImplicits._

class MyReceiver() extends Receiver {
  MidiSystem.getMidiDeviceInfo().map(a => System.out.println("MIDI info:" +  a + "//" + MidiSystem.getMidiDevice(a).getClass().getSimpleName()))
  val device = MidiSystem.getMidiDeviceInfo().map(MidiSystem.getMidiDevice(_)).find(_.getClass().getSimpleName()=="MidiInDevice").get

  val trans = device.getTransmitter()
  device.open()
  trans.setReceiver(this)
  System.out.println(device)
  System.out.println(trans)

  def close(): Unit = {trans.close()}

  def send(msg: javax.sound.midi.MidiMessage,value: Long): Unit = {
    System.out.println(msg)
    System.out.println(value)
  }

}


/**
Other polysynths can be implemented by overriding newVoice():
newVoice() needs to return the SiGen and the envelopes that need to be released on key released
newVoice() needs to setup the call back (into 'this' with the note as id) for the voice to be removed when the env is done
*/

class PolySynth extends MyReceiver with EnvCallbackDestination {

  var activeNotes = Map[Int, (SiGen, List[Env])]()

  val mx = new Mixer(List())
  val eng = new SoundEngine(mx)
  eng.start


  override def close(): Unit = {super.close(); eng.stop}

  override def send(msg: javax.sound.midi.MidiMessage,value: Long): Unit = {

    msg match {
      case a: ShortMessage => {

        a.getCommand match {

          case ShortMessage.NOTE_ON => {
            System.out.println("NoteOn: ")
            System.out.println("Data: " + a.getData1())
            System.out.println("Data: " + a.getData2())
            val note = a.getData1()

            activeNotes.get(note) match { // is there already a sound on that note?
              case Some((sound, envs))  => // all ongoing notes
                //mx.sources = mx.sources.filterNot(el => el == sound) // remove it from engine
                envs.map(_.retrigger())
                // TODO: I think there is a race condition: when env finished in the same moment it is retriggered, it may still be removed from the engine

              case _ => // ok
                val (sound, envs) = newVoice(note)

                activeNotes = activeNotes + (note -> (sound, envs))
                mx.sources = sound::mx.sources
            }
          }

          case ShortMessage.NOTE_OFF => {
            System.out.println("NoteOff: ")
            System.out.println("Data: " + a.getData1())
            System.out.println("Data: " + a.getData2())
            val note = a.getData1()
            activeNotes.get(note) match {
              case Some((sound, envs)) => //mx.sources = mx.sources.filterNot(el => el == sound)
                envs.map(_.release())
                // TODO: put it in separate map, so we can clean it up later
              case _ =>

            }
          }
        }

        def newVoiceOld(note: Int):Tuple2[SiGen, List[Env]] = {
          // 2(m−69)/12(440 Hz)
          val freq =  (440f * math.pow(2, (note-69)/12f)).toFloat

          val sound1 = new VCA(new SawOsc(new ConstantValue(freq), 0.95f), new ConstantValue(0.99f))

          val freqMod = new VCA(new SinOsc(1), new ConstantValue(freq * 0.005f))
          val totalFreq = new Mixer(List(freqMod,new ConstantValue(freq + freq*0.01f)))
          val sound2 = new VCA(new SawOsc(totalFreq, 0.95f), new ConstantValue(0.99f))

          var totalSound: SiGen = new Mixer(List(sound1, sound2))
          val env = new Env(0.1f, 0.5f, 1f, 0.7f, Some(this), note)
          val filterEnv = new Env(0.2f, 1f, 0.2f, 0.9f, None, note)
          totalSound = new Digital2Pole(totalSound, new VCA(filterEnv, new ConstantValue(0.3f)), new ConstantValue(1.7f))
          totalSound = new VCA(totalSound, env)

          return (totalSound, List(env, filterEnv))
        }


      }

      case _ => None
    }
  }

  def newVoice(note: Int):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    val freq =  (440f * math.pow(2, (note-69)/12f)).toFloat

    val sound1 = new SawOsc(freq, 0.95f) //* 0.99f

    val freqMod = new SinOsc(1) *  (freq * 0.005f)
    val totalFreq = freqMod + (freq + freq*0.01f)
    val sound2 = new SawOsc(totalFreq, 0.95f) //* 0.99f

    var totalSound: SiGen = sound1 + sound2
    val env = new Env(0.1f, 0.5f, 1f, 0.7f, Some(this), note)
    val filterEnv = new Env(0.2f, 1f, 0.3f, 1.5f, None, note)
    totalSound = new Digital2Pole(totalSound, filterEnv * 0.3f, 0.6f)
    totalSound = totalSound *  env

    return (totalSound, List(env, filterEnv))
  }

  override def envelopeDone(callbackIdentifier: Int) = {
    val note = callbackIdentifier
    activeNotes.get(note) match { // is there a sound on that note?
      case None => System.out.println("env done but no sound found")
      case Some((sound, _)) => this.synchronized {
        mx.sources = mx.sources.filterNot(el => el == sound) // remove it from engine
        activeNotes = activeNotes - note
      }
        System.out.println("env done, removed sound")
      case _ =>
    }
  }
}
