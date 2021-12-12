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
}
