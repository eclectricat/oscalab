import MyImplicits._

class PureSynth extends PolySynth {

  val ratios: List[Float] = List(1, 13/12f, 9/8f, 6/5f, 5/4f, 4/3f, 11/8f, 3/2f, 8/5f, 5/3f, 9/5f, 15/8f)

  val mix = new ConstantValue(0.5f)

  val pMix = new ParamInfo("PureOrEqual", 0, 1, mix)
  new SliderPanel(List(pMix)).show()

  def getFreqPure(note:Int):Float = {
    var relativeToC = note - 36

    val octave = relativeToC / 12

    while (relativeToC < 0) relativeToC+=12


    val baseFreq = 65.406f // TODO, make this the C

    System.out.println("Index " + relativeToC % 12)
    System.out.println("Ratio " + ratios(relativeToC % 12))
    System.out.println("octave " + octave)
    val freq =(scala.math.pow(2, octave).toFloat) * baseFreq * ratios(relativeToC % 12)

    freq
  }
  def getFreqEqual(note:Int):Float = {
    (440f * math.pow(2, ((note)-69)/12f)).toFloat
  }

  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)

    val freq1 = getFreqEqual(note)
    val freq2 = getFreqPure(note)


    val mixed = mix * freq1 + (new ConstantValue(1f) + (mix * -1)) * freq2

    //System.out.println("Frequency:" +mixed.value + "Hz")

    //val sound1 = new SawOsc(freq, 0.99f) //* 0.99f
    val sound1 = new SinOsc(mixed)  +new SinOsc(2 * mixed) +new SinOsc(3 * mixed) //* 0.99f

    //val freqMod = new SinOsc(1) *  (freq * 0.005f)
    //val totalFreq = freqMod + (freq + freq*0.01f)
    //val sound2 = new SawOsc(totalFreq, 0.99f) //* 0.99f

    var totalSound: SiGen = sound1 //+ sound2 // * -1
    val env = new LinEnv(0.1f, 0.5f, 1f, 0.7f, Some(this), note)
    val filterEnv = new LinEnv(0.2f, 1f, 0.3f, 1.5f, None, note)
    totalSound = new Digital2Pole(totalSound, filterEnv * 0.2f, 0.3f)
    totalSound = totalSound *  env
    //totalSound = new WaveFolder(totalSound, 1f, 3)
    return (totalSound, List(env, filterEnv))
  }

}
