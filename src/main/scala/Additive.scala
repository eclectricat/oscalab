import MyImplicits._


object Additive {


  def test: SoundEngine = {

    val r = scala.util.Random
    val nbHarmonics = 32

    val baseFreq = new ConstantValue(200f)
    val detune = new ConstantValue(0f) // factor to apply to frequency
    val ampMod = new ConstantValue(0f)

    val pFreq = new ParamInfo("baseFreq", 100, 500, baseFreq)
    val pDetune = new ParamInfo("randomDetuneFactor", 0f, 0.01f, detune )
    val pAmpMod = new ParamInfo("ampModAmount", 0, 0.5f, ampMod)
    new SliderPanel(List(pFreq, pDetune, pAmpMod)).show()



    val oscs = (1 to nbHarmonics).map { harm =>


        val freq = harm.toFloat * baseFreq
        val lfo = new SinOsc(0.1f + r.nextFloat())
        val ampli = 1.0f/harm + (lfo * ampMod * (1.0f/harm))
        val ranDetune = r.nextFloat() - 0.5f // -0.5 to 0.5
        new SinOsc(freq * (1 + ranDetune * detune)) * ampli
    }

    new SoundEngine(new Mixer(oscs.toList))
  }

}
