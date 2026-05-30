package framework

import framework.MyImplicits._


class PolyDrums(deviceId:String = "", nbInstruments:Int=4) {

  val mxr = new Mixer(List())
  val e = new SoundEngine(mxr)
  //val eng = new CoreAudioEngine(mx)
  e.start()

  val drums = for(i<- 0 until nbInstruments)
    yield new Drums(deviceId, midiChannel=Some(i), sharedMixer=Some(mxr))

  def close() = {
    drums.map(_.close())
    e.stop()
  }

  def savePatch(folderPath: String) = { // typically provide a folder name here
    drums.foreach { d =>
      val fn = "drums_channel_"+ d.midiChannel.getOrElse(99)+ ".json"
      d.sliderPanel.saveParams(Some(folderPath+"/"+fn))
    }
  }

  def loadPatch(prefixPath: String) = {
    for (i <- 0 until nbInstruments) {
      val fn = "drums_channel_"+ i + ".json"
      drums.toList(i).sliderPanel.loadParamsFromFile(prefixPath+fn)
    }
  }

}

class Drums(deviceId:String = "", midiChannel: Option[Int]=None, sharedMixer: Option[Mixer]=None) extends Poly(deviceId, midiChannel, sharedMixer) {

  val cutoff = new  ConstantValue(0f)
  val reso = new ConstantValue(0f)

  val cutoffHp = new  ConstantValue(0f)

  val pitchRelease = new ConstantValue(1f)
  val noiseAmpRelease = new ConstantValue(1f)
  val sinAmpRelease = new ConstantValue(1f)
  val filterRelease = new ConstantValue(1f)
  val clickAmpRelease = new ConstantValue(0.1f)

  val fmLevel = new ConstantValue(0f)
  val fmRatio = new ConstantValue(1f)


  val pitchEnvAmount = new ConstantValue(0.3f)
  val filterEnvAmount = new ConstantValue(0.3f)

  val sinLevel = new ConstantValue(1f)
  val noiseLevel = new ConstantValue(1f)
  val clickLevel = new ConstantValue(1f)


  val pCutoff = new ParamInfo("cutoff", 0, 1, cutoff)
  val pReso = new ParamInfo("reso", 0, 1, reso)

  val pCutoffHp = new ParamInfo("cutoffHp", 0, 1, cutoffHp)

  val pPitchRelease = new ParamInfo("pitchRelease", 0, 1, pitchRelease)
  val pNoiseAmpRelease = new ParamInfo("noiseAmpRelease", 0, 2, noiseAmpRelease)

  val pSinAmpRelease = new ParamInfo("sinAmpRelease", 0, 2, sinAmpRelease)
  val pFilterRelease = new ParamInfo("filterRelease", 0, 2, filterRelease)
  val pClickAmpRelease = new ParamInfo("clickAmpRelease", 0, 0.5f, clickAmpRelease)

  val pFmLevel = new ParamInfo("fm level", 0, 1f, fmLevel)
  val pFmRatio = new ParamInfo("fm ratio octave", -3, 3, fmRatio)


  val pFilterEnvAmount = new ParamInfo("filterEnvAmount", -1, 1, filterEnvAmount)
  val pPitchEnvAmount = new ParamInfo("pitchEnvAmount", 0, 1, pitchEnvAmount)
  val pSinLevel = new ParamInfo("sinLevel", 0,1,sinLevel)
  val pNoiseLevel = new ParamInfo("noiseLevel", 0,1,noiseLevel)
  val pClickLevel = new ParamInfo("clickLevel", 0,1,clickLevel)

  val UITone = new PanelDividerUI("**** TONE *****")
  val UINoise = new PanelDividerUI("**** NOISE *****")
  val UIClick = new PanelDividerUI("**** CLICK *****")

  val sliderPanel = new SliderPanel(List(UITone, pPitchRelease,pPitchEnvAmount,pSinAmpRelease,pSinLevel,pFmLevel,pFmRatio,
    UINoise, pNoiseAmpRelease, pNoiseLevel,
    pCutoff,pReso,pFilterRelease,pFilterEnvAmount,pCutoffHp,
  UIClick, pClickAmpRelease, pClickLevel))
  sliderPanel.show()

  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    val freq =  (440f * math.pow(2, ((note)-69)/12f)).toFloat

    val sinAmpEnv = new ExpEnv(0, sinAmpRelease, 0 , sinAmpRelease, Some(this), note)
    val noiseAmpEnv = new ExpEnv(0, noiseAmpRelease, 0 , noiseAmpRelease, Some(this), note)
    val clickAmpEnv = new ExpEnv(0, clickAmpRelease, 0 , clickAmpRelease, None, note)
    val pitchEnv = new ExpEnv(0, pitchRelease, 0 , pitchRelease, None, note)
    val filterEnv = new ExpEnv(0, filterRelease, 0 , filterRelease, None, note)

    val baseFreq = freq + pitchEnv * pitchEnvAmount*1000f
    val fmFreq = baseFreq * math.pow(2, fmRatio.getValue(0)).toFloat

    val fmOsc = new SinOsc(fmFreq)

    val sin = new SinOsc(baseFreq + fmOsc * fmLevel * 1000f)
    val noise = new NoiseOsc()

    // noise
    var totalSound: SiGen = noise * noiseLevel * noiseAmpEnv
    totalSound = new SKF_OM_FB(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    totalSound = new Digital2PoleHP(totalSound, cutoffHp, 0f)
    // tone
    totalSound = totalSound + sin * sinLevel * sinAmpEnv
    // click
    totalSound = totalSound + noise * clickLevel * clickAmpEnv
    return (totalSound, List(sinAmpEnv,noiseAmpEnv,pitchEnv, filterEnv, clickAmpEnv))
  }

  override def envelopeDone(callbackIdentifier: Int) = {
    val note = callbackIdentifier
    activeNotes.get(note) match { // is there a sound on that note?
      case None => System.out.println("env done but no sound found")
      case Some((sound, envs)) =>  if (envs.forall(env => env.canBeKilled())) {
        this.synchronized {
          mx.sources = mx.sources.filterNot(el => el == sound) // remove it from engine
          activeNotes = activeNotes - note
        }
        System.out.println("all envs done, removed sound")
      } else System.out.println("not all envs done")

      case _ =>
    }
  }

  override def close() = {
    super.close()
    sliderPanel.close()
  }

}
