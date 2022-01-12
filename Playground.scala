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

  def formant: SoundEngine = {

    val osc = new Mixer(List(new SawOsc(110, 0.99f), new SawOsc(111, 0.999f)))

    // List[Tuple2[Tuple3[ConstantValue, ConstantValue, ConstantValue], SiGen]]
    val cValues = List(0.09f, 0.19f, 0.34f).map { f =>
      val cutoff = new ConstantValue(f)
      val reso = new ConstantValue(0.92f)
      val scale = new ConstantValue(0.5f)
      val pCut = new ParamInfo("Cutoff", 0, 1, cutoff)
      val pReso = new ParamInfo("Res", 0, 1, reso)
      val pScale = new ParamInfo("Scale", 0, 1, scale)
      val filter = new Digital2PoleHP(osc, cutoff, reso)

      (List(pCut, pReso, pScale),filter)

    }.toList


    new SliderPanel(cValues.map(_._1).flatten).show()

    new SoundEngine(new Mixer(cValues.map(v => v._2 * v._1(2).value)))


  }

  def panTest:SoundEngine = {
    val osc = new SawOsc(440, 0.9f)
    val pan = new PanBalance(osc, -1f)
    val eng = new SoundEngine(pan)
    return eng
  }

  def foldingTest: SoundEngine = {
    val scale = new ConstantValue(1f)
    val pScale = new ParamInfo("Folding Scala, 0:4", 0, 4,  scale)

    val osc2 = new ConstantValue(1f)
    val pOsc2 = new ParamInfo("osc 2", 0,1,  osc2)

    val panel = new SliderPanel(List(pScale, pOsc2))
    panel.show
    val fold = new WaveFolder(new SawOsc(150, 0.99f) + osc2 * new SawOsc(151, 0.99f), scale, 2)
    //val fold = new WaveFolder(new SinOsc(150), scale, 4)
    val eng = new SoundEngine(fold)
    eng
  }

  def fmTest: SoundEngine = {
    val fMod = new ConstantValue(40)
    val pFmod = new ParamInfo("fmod", 1, 100,  fMod)

    val aMod = new ConstantValue(1)
    val pAmod = new ParamInfo("aMod", 0, 400,  aMod)

    val panel = new SliderPanel(List(pFmod, pAmod))
    panel.show
    val fm = new SinOsc(150 + aMod * new SinOsc(fMod))
    //val fold = new WaveFolder(new SinOsc(150), scale, 4)
    val eng = new SoundEngine(fm)
    eng
  }

  def doubleFMWF: SoundEngine = {
    val fMod1 = new ConstantValue(40)
    val pFmod1 = new ParamInfo("fmod1", 1, 100,  fMod1)

    val aMod1 = new ConstantValue(1)
    val pAmod1 = new ParamInfo("aMod1", 0, 400,  aMod1)

    val fMod2 = new ConstantValue(40)
    val pFmod2 = new ParamInfo("fmod2", 1, 100,  fMod2)

    val aMod2 = new ConstantValue(1)
    val pAmod2 = new ParamInfo("aMod2", 0, 400,  aMod2)

    val scale = new ConstantValue(1f)
    val pScale = new ParamInfo("Folding Scala, 1:4", 0, 4,  scale)

    val panel = new SliderPanel(List(pFmod1, pAmod1, pFmod2, pAmod2, pScale))
    panel.show
    val fm1 = new SinOsc(150 + aMod1 * new SinOsc(fMod1))
    val fm2 = new SinOsc(150 + aMod2 * new SinOsc(fMod2))
    //val fold = new WaveFolder(new SinOsc(150), scale, 4)
    val eng = new SoundEngine(new WaveFolder(new Mixer(List(fm1, fm2)), scale, 4))
    eng
  }
}
