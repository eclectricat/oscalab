package framework

import framework.MyImplicits._

import javax.sound.midi._

class MonoSynth(deviceId:String = "") extends MyReceiver(deviceId) with StandardKnobs {

  val freq = new ConstantValue(100f)
  val r = scala.util.Random
  var (sound,envs)  = newVoice(new Portamento(freq, 1 + 0.6f/40000, noiseAmount = 0f))
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

            envs.map { e =>
              e.atk = attack.value
              e.rel = release.value
              e.dec = release.value
              e.retrigger()
            }

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



  def newVoice(freq: SiGen, detune: Float=0):Tuple2[SiGen, List[LinEnv]] = {
    // 2(m−69)/12(440 Hz)
    //val freq =  (440f * math.pow(2, ((note+detune)-69)/12f)).toFloat

    val sound1 = new SawOscB(freq, initialPhase=r.nextFloat()) //* 0.99f

    //val freqMod = new SinOsc(1) *  (freq * 0.005f)
    //val totalFreq = freqMod + (freq + freq*0.01f)
    val totalFreq = (freq + 0.5f)
    val sound2 = new SawOscB(totalFreq, initialPhase=r.nextFloat()) //* 0.99f

    var totalSound: SiGen = sound1 + sound2 * osc2level // * -1
    val env = new LinEnv(attack.getValue(0), 0.5f, 1f, release.getValue(0), None, 0)
    val filterEnv = new LinEnv(attack.getValue(0), release.getValue(0), 0.3f, release.getValue(0), None, 0)
    totalSound = new Digital2Pole(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    totalSound = totalSound *  env
    //totalSound = new WaveFolder(totalSound, 1f, 3)
    return (totalSound, List(env, filterEnv))
  }


}



class SynthWaveBass(deviceId:String = "") extends MyReceiver(deviceId)  {

  val freq = new ConstantValue(100f)
  val r = scala.util.Random


  val cutoff = new  ConstantValue(0.3f)
  val reso = new ConstantValue(0f)
  val attack = new ConstantValue(0.01f)
  val release = new ConstantValue(1f)
  val filterEnvAmount = new ConstantValue(0.3f)
  val subLevel = new ConstantValue(1f)
  val subPhase = new ConstantValue(0f)
  val unisonDetune = new ConstantValue(0f)

  def showUI():Unit = {
    //class ParamInfo(val name:String, val min:Float, val max:Float, val value:ConstantValue)
    val pCutoff = new ParamInfo("cutoff", 0, 1, cutoff)
    val pReso = new ParamInfo("reso", 0, 1, reso)
    val pAttack = new ParamInfo("attack", 0.0f, 0.2f, attack)
    val pRelease = new ParamInfo("release", 0, 3, release)
    val pFilterEnvAmount = new ParamInfo("filterEnv", 0, 1, filterEnvAmount)
    val pSubLevel = new ParamInfo("SubLevel", 0,1,subLevel)
    val pSubPhase = new ParamInfo("SubPhase", 0,1,subPhase)
    val pUnisonDetune = new ParamInfo("unison detune", 0,2,unisonDetune)

    new SliderPanel(List(pCutoff, pReso, pAttack, pRelease, pFilterEnvAmount, pSubLevel, pSubPhase, pUnisonDetune)).show()

  }


  val nbVoices = 6

  var (sounds:List[SiGen], listOfEnvs:List[List[ExpEnv]]) = (1 to nbVoices).map { voiceNr =>
    newVoice(freq + r.nextFloat() * unisonDetune, sub= (voiceNr >= 0))
  }.toList.unzip

  val envs:List[ExpEnv] = listOfEnvs.flatten


  //var (sound,envs)  = newVoice(freq)
  envs.map(_.state=0)

  var lastPressedNote = -1

  //val mx = new Mixer(List(sound))
  val mx = new Mixer(sounds)
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

            envs.map { e =>
              e.atk = attack.value
              e.rel = release.value
              e.dec = release.value
              e.recalculateExpFactors()
              e.retrigger()
            }

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

  def newVoice(freq: SiGen, detune: Float=0, sub:Boolean=false):Tuple2[SiGen, List[ExpEnv]] = {
    // 2(m−69)/12(440 Hz)
    //val freq =  (440f * math.pow(2, ((note+detune)-69)/12f)).toFloat

    System.out.println("sub:" + sub)
    val sound1 = new SawOscB(freq, initialPhase = r.nextFloat() ) //* 0.99f

    //val freqMod = new SinOsc(1) *  (freq * 0.005f)
    //val totalFreq = freqMod + (freq + freq*0.01f)
    val totalFreq = (freq + 0.5f)
    val sound2 = new PulseOscB(freq * 0.5f, phaseOffset=subPhase) //* 0.99f

    var totalSound: SiGen = if (sub) (sound1 + sound2 * subLevel) else sound1  // * -1

    val env = new ExpEnv(attack.getValue(0), release.getValue(0), 0f, release.getValue(0), None, 0)
    val filterEnv = new ExpEnv(attack.getValue(0), release.getValue(0), 0f, release.getValue(0), None, 0)
    totalSound = new Digital2Pole(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    //totalSound = new Digital4PoleZDF(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    totalSound = totalSound *  env
    //totalSound = new WaveFolder(totalSound, 1f, 3)
    return (totalSound, List(env, filterEnv))
  }




}

class SynthWaveBassStereo(deviceId:String = "") extends MyReceiver(deviceId)  {

  val freq = new ConstantValue(100f)
  val r = scala.util.Random


  val cutoff = new  ConstantValue(0.3f)
  val reso = new ConstantValue(0f)
  val attack = new ConstantValue(0.01f)
  val release = new ConstantValue(1f)
  val filterEnvAmount = new ConstantValue(0.3f)
  val subLevel = new ConstantValue(1f)
  val subPhase = new ConstantValue(0f)
  val unisonDetune = new ConstantValue(0f)

  def showUI():Unit = {
    //class ParamInfo(val name:String, val min:Float, val max:Float, val value:ConstantValue)
    val pCutoff = new ParamInfo("cutoff", 0, 1, cutoff)
    val pReso = new ParamInfo("reso", 0, 1, reso)
    val pAttack = new ParamInfo("attack", 0.0f, 0.2f, attack)
    val pRelease = new ParamInfo("release", 0, 3, release)
    val pFilterEnvAmount = new ParamInfo("filterEnv", 0, 1, filterEnvAmount)
    val pSubLevel = new ParamInfo("SubLevel", 0,1,subLevel)
    val pSubPhase = new ParamInfo("SubPhase", 0,1,subPhase)
    val pUnisonDetune = new ParamInfo("unison detune", 0,7,unisonDetune)

    new SliderPanel(List(pCutoff, pReso, pAttack, pRelease, pFilterEnvAmount, pSubLevel, pSubPhase, pUnisonDetune)).show()

  }


  var (sound, envs) = newVoice(freq)

  //var (sound,envs)  = newVoice(freq)
  envs.map(_.state=0)

  var lastPressedNote = -1

  //val mx = new Mixer(List(sound))
  val eng = new CoreAudioEngine(sound)
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

            envs.map { e =>
              e.atk = attack.value
              e.rel = release.value
              e.dec = release.value
              e.recalculateExpFactors()
              e.retrigger()
            }

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

  def newVoice(freq: SiGen):Tuple2[SiGen, List[ExpEnv]] = {
    // 2(m−69)/12(440 Hz)
    //val freq =  (440f * math.pow(2, ((note+detune)-69)/12f)).toFloat

    val nbSaws = 4

    val sound1 = new SawOscB(freq) //* 0.99f

    val leftSaws: List[SiGen] = (1 to nbSaws).map {i => new SawOscB(freq + r.nextFloat() * unisonDetune)}.toList
    val rightSaws: List[SiGen] = (1 to nbSaws).map {i => new SawOscB(freq + r.nextFloat() * unisonDetune)}.toList

    val sub:SiGen = new PulseOscB(freq * 0.5f, phaseOffset=subPhase) * subLevel //* 0.99f



    val env = new ExpEnv(attack.getValue(0), release.getValue(0), 0f, release.getValue(0), None, 0)
    val filterEnv = new ExpEnv(attack.getValue(0), release.getValue(0), 0f, release.getValue(0), None, 0)

    var leftChannel = new Digital2Pole(new Mixer(sub::leftSaws), filterEnv * filterEnvAmount + cutoff, reso) * env
    var rightChannel = new Digital2Pole(new Mixer(sub::rightSaws), filterEnv * filterEnvAmount + cutoff, reso) * env
    val stereoSound: SiGen = new PanBalance(leftChannel, -1f) + new PanBalance(rightChannel, 1f)

    return (stereoSound, List(env, filterEnv))
  }




}
