import MyImplicits._

object Playground {

  def sliderTest:SoundEngine = {
    val cutoff = new ConstantValue(0.5f)
    val reso = new ConstantValue(0f)
    val detune = new ConstantValue(0f)
    val saw2scale = new ConstantValue(1f)

    val pCut = new ParamInfo("Cutoff", 0, 1, cutoff)
    val pReso = new ParamInfo("Res", 0, 1, reso)
    val pDetune = new ParamInfo("Detune", -2, 2, detune)
    val pSaw2Scale = new ParamInfo("Saw 2 scale, -1:1", -1, 1,  saw2scale)

    val panel = new SliderPanel(List(pCut, pReso, pDetune, pSaw2Scale))
    panel.show

    val osc = new SawOsc(110, 0.99f) + (saw2scale * new SawOsc(110 + detune, 0.99f))
    val filter = new Digital2Pole(osc, cutoff, reso)

    val eng = new SoundEngine(filter)
    return eng
  }

  def panTest:SoundEngine = {
    val osc = new SawOsc(440, 0.9f)
    val pan = new PanBalance(osc, -1f)
    val eng = new SoundEngine(pan)
    return eng
  }

  def foldingTest: SoundEngine = {
    val scale = new ConstantValue(1f)
    val pScale = new ParamInfo("Folding Scala, 1:4", 1, 4,  scale)

    val panel = new SliderPanel(List(pScale))
    panel.show
    val fold = new WaveFolder(new SawOsc(150, 0.99f), scale, 2)
    //val fold = new WaveFolder(new SinOsc(150), scale, 4)
    val eng = new SoundEngine(fold)
    eng
  }
}
