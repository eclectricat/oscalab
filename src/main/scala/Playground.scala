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

    //val osc = new SawOsc(110, 0.99f) + (saw2scale * new SawOsc(110 + detune, 0.99f))
    val osc = new SawOscB(110) + (saw2scale * new SawOsc(110 + detune, 0.99f))
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
    //val fold = new WaveFolder(new SawOsc(150, 0.99f) + osc2 * new SawOsc(151, 0.99f), scale, 1)
    val fold = new WaveFolder(new SinOsc(150)+osc2, scale, 1)
    //val fold = new WaveFolderSqueeze(new SinOsc(150) + osc2 * new SinOsc(1), scale)
    //val fold = new WaveFolderSqueeze(new SawOsc(150, 0.99f) + osc2 * new SinOsc(1), scale)
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

  def pulseWidthTest: SoundEngine = {
    val modAmount = new ConstantValue(40)
    val pModAmound = new ParamInfo("modAmount", -700 , 700,  modAmount)

    val panel = new SliderPanel(List(pModAmound))
    panel.show
    val lfo = new SinOsc(0.5f)
    val s = new SawOscB(150f + modAmount  * new SinOsc(150f))
    //val s = new SinOsc(150f + modAmount * lfo * new SawOsc(150f, 1f))
    //val s = new SinOsc(150f + modAmount  * new SawOsc(150f, 0.99f))
    //val s = new PulseOsc(150f + modAmount * lfo * new SinOsc(150f))
    //val s = new PulseOsc(150f + modAmount  * new SinOsc(150f))

    //val s = new SinOsc(150f + modAmount  * new SinOsc(150f))


    val eng = new SoundEngine(s)
    eng


  }

  def aliasingTest: SoundEngine = {
    val freq = new ConstantValue(5000)
    val pFreq = new ParamInfo("freq", 1000 , 10000,  freq)

    val o1 = new ConstantValue(1f)
    val pO1 = new ParamInfo("o1 vol", 0 , 1,  o1)

    val o2 = new ConstantValue(1f)
    val pO2 = new ParamInfo("o2 vol", 0 , 1,  o2)

    val o3 = new ConstantValue(1f)
    val pO3 = new ParamInfo("o3 vol", 0 , 1,  o3)

    val panel = new SliderPanel(List(pFreq, pO1, pO2, pO3))
    panel.show

    val eng = new SoundEngine(new SawOsc(freq, 0.90f) * o1 + new SawOscB(freq) * o2 + new SawOscB(freq, blep=false) * o3 )
    eng

  }

  def fmPluck: SoundEngine = {
    /**val freq = new ConstantValue(5000)
    val pFreq = new ParamInfo("freq", 1000 , 10000,  freq)

    val o1 = new ConstantValue(1f)
    val pO1 = new ParamInfo("o1 vol", 0 , 1,  o1)

    val o2 = new ConstantValue(1f)
    val pO2 = new ParamInfo("o2 vol", 0 , 1,  o2)
    **/
    val modDepth = new ConstantValue(1f)
    val pModDepth = new ParamInfo("mode depth", 0 , 1000,  modDepth)

    val modDepth2 = new ConstantValue(1f)
    val pModDepth2 = new ParamInfo("mode depth2", 0 , 1000,  modDepth2)

    val panel = new SliderPanel(List(pModDepth,pModDepth2))
    panel.show


   val lfo1 = new SawOsc(1f, 0.99f)
   val lfo2 = new SawOsc(1f, 0.99f)

   var freqMod = modDepth * new SinOsc(500f) * ((lfo2* -1) + 1)
   var freqMod2 = modDepth2 * new SinOsc(100f) * ((lfo2* -1) + 1)

   val sin1 = new SinOsc(100f + freqMod+freqMod2)



    val eng = new SoundEngine(sin1 * ((lfo1* -1) + 1))
    //val eng = new SoundEngine(freqMod)
    eng

  }


}
