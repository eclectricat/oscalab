package framework

import framework.MyImplicits._

import javax.sound.midi._

class MyReceiver(deviceId:String = "") extends Receiver {
  MidiSystem.getMidiDeviceInfo().map(a => System.out.println("MIDI info:" +  a + "//" + MidiSystem.getMidiDevice(a).getClass().getSimpleName()))
  //val device = MidiSystem.getMidiDeviceInfo().map(MidiSystem.getMidiDevice(_)).find(_.getClass().getSimpleName()=="MidiInDevice").get
  val deviceInfos = MidiSystem.getMidiDeviceInfo().filter(_.toString().contains(deviceId))
  val device = deviceInfos.map(MidiSystem.getMidiDevice(_)).find(_.getClass().getSimpleName()=="MidiInDevice").get //OrElse(System.out.println("No device found"))


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

trait StandardKnobs {

  val cutoff = new  ConstantValue(0f)
  val reso = new ConstantValue(0f)
  val attack = new ConstantValue(0.2f)
  val release = new ConstantValue(1f)
  val filterEnvAmount = new ConstantValue(0.3f)
  val osc2level = new ConstantValue(1f)

  def showUI():Unit = {
    //class ParamInfo(val name:String, val min:Float, val max:Float, val value:ConstantValue)
    val pCutoff = new ParamInfo("cutoff", 0, 1, cutoff)
    val pReso = new ParamInfo("reso", 0, 1, reso)
    val pAttack = new ParamInfo("attack", 0, 2, attack)
    val pRelease = new ParamInfo("release", 0, 3, release)
    val pFilterEnvAmount = new ParamInfo("filterEnv", 0, 1, filterEnvAmount)
    val pOsc2Level = new ParamInfo("osc2Level", 0,1,osc2level)

    new SliderPanel(List(pCutoff, pReso, pAttack, pRelease, pFilterEnvAmount, pOsc2Level)).show()

  }


}

class PolySynth(deviceId:String = "") extends MyReceiver(deviceId) with EnvCallbackDestination with StandardKnobs {

  var activeNotes = Map[Int, (SiGen, List[Env])]()
  val r = scala.util.Random

  val mx = new Mixer(List())
  val eng = new SoundEngine(mx)
  //val eng = new CoreAudioEngine(mx)
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
          val env = new LinEnv(attack.getValue(0), 0.5f, 1f, release.getValue(0), Some(this), note)
          val filterEnv = new LinEnv(attack.getValue(0), release.getValue(0), 0.2f, release.getValue(0), None, note)
          //totalSound = new Digital2Pole(totalSound, new VCA(filterEnv, new ConstantValue(0.3f)), new ConstantValue(1.7f))
          totalSound = new SKF_OM(totalSound, new VCA(filterEnv, new ConstantValue(0.3f)), new ConstantValue(1.7f))
          totalSound = new VCA(totalSound, env)

          return (totalSound, List(env, filterEnv))
        }


      }

      case _ => None
    }
  }

  def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    val freq =  (440f * math.pow(2, ((note+detune)-69)/12f)).toFloat

    val sound1 = new SawOscB(freq, initialPhase=r.nextFloat()) //* 0.99f

    //val freqMod = new SinOsc(1) *  (freq * 0.005f)
    //val totalFreq = freqMod + (freq + freq*0.01f)
    val totalFreq = (freq + 0.5f)
    val sound2 = new SawOscB(totalFreq, initialPhase=r.nextFloat()) //* 0.99f

    var totalSound: SiGen = sound1 + sound2 * osc2level // * -1
    val env = new LinEnv(attack.getValue(0), 0.5f, 1f, release.getValue(0), Some(this), note)
    val filterEnv = new LinEnv(attack.getValue(0), release.getValue(0), 0.3f, release.getValue(0), None, note)
    //totalSound = new Digital2Pole(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    //totalSound = new Mystran(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    totalSound = new Digital4PoleFP(totalSound, filterEnv * filterEnvAmount + cutoff, reso, "skf")
    totalSound = totalSound *  env
    //totalSound = new WaveFolder(totalSound, 1f, 3)
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

class BinauralSynth(deviceId:String = "MS") extends PolySynth(deviceId) {

  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    System.out.println("overriden")
    val (l, envs1) = super.newVoice(note, -0.1f)
    val (r, envs2) = super.newVoice(note, 0.1f)

    val stereoSound: SiGen = (new PanBalance(l, -1f) + new PanBalance(r, 1f))
    val allEnvs: List[Env] = envs1 ++ envs2
    return (stereoSound, allEnvs)
  }

}

abstract class Poly(deviceId:String = "", val midiChannel: Option[Int]=None, sharedMixer: Option[Mixer]=None) extends MyReceiver(deviceId) with EnvCallbackDestination {

  var activeNotes = Map[Int, (SiGen, List[Env])]()
  val r = scala.util.Random

  var eng:Option[SoundEngine] = None

  val mx = sharedMixer match {
    case Some(mixer) => mixer
    case _ =>
       val mxr = new Mixer(List())
       val e = new SoundEngine(mxr)
       //val eng = new CoreAudioEngine(mx)
       e.start()
       eng = Some(e)
       mxr
  }



  //showUI()

  override def close(): Unit = {super.close(); eng.map(_.stop())}

  override def send(msg: javax.sound.midi.MidiMessage,value: Long): Unit = {

    msg match {
      case a: ShortMessage if midiChannel.map(_ == a.getChannel()).getOrElse(true)  => {

        a.getCommand match {

          case ShortMessage.NOTE_ON => {
            System.out.println("NoteOn: ")
            System.out.println("Data: " + a.getData1())
            System.out.println("Data: " + a.getData2())
            System.out.println("Channel: " + a.getChannel())
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

      }

      case _ => None
    }
  }

  def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]]

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

class Junolike(deviceId:String = "") extends Poly(deviceId) {

  val cutoff = new  ConstantValue(0f)
  val reso = new ConstantValue(0f)
  val attack = new ConstantValue(0.2f)
  val release = new ConstantValue(1f)
  val sustain = new ConstantValue(0.5f)
  val filterEnvAmount = new ConstantValue(0.3f)
  val sawLevel = new ConstantValue(1f)
  val pulseLevel = new ConstantValue(1f)
  val subLevel = new ConstantValue(1f)

  val pCutoff = new ParamInfo("cutoff", 0, 1, cutoff)
  val pReso = new ParamInfo("reso", 0, 1, reso)
  val pAttack = new ParamInfo("attack", 0, 2, attack)
  val pRelease = new ParamInfo("release", 0, 5, release)
  val pSustain = new ParamInfo("sustain", 0, 1, sustain)

  val pFilterEnvAmount = new ParamInfo("filterEnv", 0, 1, filterEnvAmount)
  val pSawLevel = new ParamInfo("saw", 0,1,sawLevel)
  val pPulseLevel = new ParamInfo("pulse", 0,1,pulseLevel)
  val pSubLevel = new ParamInfo("sub", 0,1,subLevel)

  new SliderPanel(List(pCutoff, pReso, pAttack, pSustain, pRelease, pFilterEnvAmount, pSawLevel, pPulseLevel, pSubLevel)).show()

  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    val freq =  (440f * math.pow(2, ((note)-69)/12f)).toFloat

    val initialPhase_ = 0 // r.nextFloat()
    val saw = new SawOscB(freq, initialPhase=initialPhase_) //* 0.99f

    val lfo = new SinOsc(1)
    val pw = lfo * 0.3f + 0.5f
    val pulse = new PulseOscB(freq, pulseWidth = pw) //* 0.99f

    val sub = new PulseOscB(freq/2f) //* 0.99f


    var totalSound: SiGen = saw * sawLevel + pulse * pulseLevel + sub * subLevel * -1  // * -1
    val env = new ExpEnv(attack.getValue(0), release.getValue(0), sustain.getValue(0), release.getValue(0), Some(this), note)
    //val filterEnv = new LinEnv(attack.getValue(0), release.getValue(0), 0.3f, release.getValue(0), None, note)

    //totalSound = new Digital4PoleZDF(totalSound, env * filterEnvAmount + cutoff, reso)
    //totalSound = new Digital4PoleZDF(totalSound, env * filterEnvAmount + cutoff, reso)
    //totalSound = new Mystran(totalSound, env * filterEnvAmount + cutoff, reso)
    //totalSound = new SKF_OM_Diodes(totalSound, env * filterEnvAmount + cutoff, reso)
    totalSound = new Digital4PoleFP(totalSound, env * filterEnvAmount + cutoff, reso, "ota4p")
    //totalSound = new SKF_OM_FB(totalSound, env * filterEnvAmount + cutoff, reso)
    totalSound = totalSound *  env
    //totalSound = new WaveFolder(totalSound, 1f, 3)
    return (totalSound, List(env))
  }

}

class ProudlyDigital(deviceId:String = "") extends Poly(deviceId) {


  val attack = new ConstantValue(0.2f)
  val release = new ConstantValue(1f)
  val sustain = new ConstantValue(0.5f)
  val detuneLevel = new ConstantValue(1f)
  val detuneSpeed = new ConstantValue(1f)
  //val tremoloLevel = new ConstantValue(1f)
  val tremoloSpeed = new ConstantValue(1f)



  val pAttack = new ParamInfo("attack", 0, 2, attack)
  val pRelease = new ParamInfo("release", 0, 5, release)
  val pSustain = new ParamInfo("sustain", 0, 1, sustain)

  val pDetuneLevel = new ParamInfo("detuneL", 0, 2, detuneLevel)
  val pDetuneSpeed = new ParamInfo("detuneSpeed", 0.1f, 10f, detuneSpeed)
  //val pTremoloLevel = new ParamInfo("tremoloL", 0, 2)
  val pTremoloSpeed = new ParamInfo("tremoloSpeed", 0.1f, 10, tremoloSpeed)



  new SliderPanel(List( pAttack, pSustain, pRelease, pDetuneLevel, pDetuneSpeed, pTremoloSpeed)).show()

  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    val freq =  (440f * math.pow(2, ((note)-69)/12f)).toFloat

    val freqs = (1 to 10).map(_ => freq * (Math.round(r.nextFloat() * 10)+1))

    var mix: SiGen = new Mixer(freqs.map(f =>
      new PanBalance(
        new SinOsc(f + 10f * detuneLevel * new SinOsc(detuneSpeed * 0.1f * r.nextFloat()))
         * new SinOsc(r.nextFloat() * 0.3f * tremoloSpeed, phaseOffset = 1.4f) * (freq/f),
        -1f + r.nextFloat()* 2f)
    ).toList)

    val env = new ExpEnv(attack.getValue(0), release.getValue(0), sustain.getValue(0), release.getValue(0), Some(this), note)

    mix = mix *  env

    return (mix, List(env))
  }

}
