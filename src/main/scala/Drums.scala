import MyImplicits._

class Drums(deviceId:String = "") extends Poly(deviceId) {

  val cutoff = new  ConstantValue(0f)
  val reso = new ConstantValue(0f)

  val pitchRelease = new ConstantValue(1f)
  val noiseAmpRelease = new ConstantValue(1f)
  val sinAmpRelease = new ConstantValue(1f)
  val filterRelease = new ConstantValue(1f)


  val pitchEnvAmount = new ConstantValue(0.3f)
  val filterEnvAmount = new ConstantValue(0.3f)

  val sinLevel = new ConstantValue(1f)
  val noiseLevel = new ConstantValue(1f)


  val pCutoff = new ParamInfo("cutoff", 0, 1, cutoff)
  val pReso = new ParamInfo("reso", 0, 1, reso)

  val pPitchRelease = new ParamInfo("pitchRelease", 0, 1, pitchRelease)
  val pNoiseAmpRelease = new ParamInfo("noiseAmpRelease", 0, 2, noiseAmpRelease)

  val pSinAmpRelease = new ParamInfo("sinAmpRelease", 0, 2, sinAmpRelease)
  val pFilterRelease = new ParamInfo("filterRelease", 0, 2, filterRelease)


  val pFilterEnvAmount = new ParamInfo("filterEnvAmount", -1, 1, filterEnvAmount)
  val pPitchEnvAmount = new ParamInfo("pitchEnvAmount", 0, 1, pitchEnvAmount)
  val pSinLevel = new ParamInfo("sinLevel", 0,1,sinLevel)
  val pNoiseLevel = new ParamInfo("noiseLevel", 0,1,noiseLevel)


  new SliderPanel(List(pPitchRelease,pPitchEnvAmount,pSinAmpRelease,pSinLevel,
    pNoiseAmpRelease, pNoiseLevel,
    pCutoff,pReso, pFilterRelease,pFilterEnvAmount)).show()

  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)
    val freq =  (440f * math.pow(2, ((note)-69)/12f)).toFloat

    val sinAmpEnv = new ExpEnv(0, sinAmpRelease.getValue(0), 0 , sinAmpRelease.getValue(0), Some(this), note)
    val noiseAmpEnv = new ExpEnv(0, noiseAmpRelease.getValue(0), 0 , noiseAmpRelease.getValue(0), Some(this), note)
    val pitchEnv = new ExpEnv(0, pitchRelease.getValue(0), 0 , pitchRelease.getValue(0), Some(this), note)
    val filterEnv = new ExpEnv(0, filterRelease.getValue(0), 0 , filterRelease.getValue(0), Some(this), note)

    val sin = new SinOsc(freq + pitchEnv * pitchEnvAmount*1000f)
    val noise = new NoiseOsc()

    var totalSound: SiGen = sin * sinLevel * sinAmpEnv + noise * noiseLevel * noiseAmpEnv


    totalSound = new SKF_OM_FB(totalSound, filterEnv * filterEnvAmount + cutoff, reso)
    return (totalSound, List(sinAmpEnv,noiseAmpEnv,pitchEnv, filterEnv))
  }

}
