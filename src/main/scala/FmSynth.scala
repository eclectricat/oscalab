import MyImplicits._

class FmSynth(deviceId:String = "") extends PolySynth(deviceId) {

  val ampEnv = new ADSRMacro("amp")

  val modEnv1 = new ADSRMacro("mod1")
  val modEnv2 = new ADSRMacro("mod2")

  val modDepth1 = new ConstantValue(1f)
  val pModDepth1 = new ParamInfo("mode depth", 0 , 1000,  modDepth1)

  val modDepth2 = new ConstantValue(1f)
  val pModDepth2 = new ParamInfo("mode depth2", 0 , 1000,  modDepth2)

  val panel = new SliderPanel(List(pModDepth1,pModDepth2) ++ ampEnv.params ++ modEnv1.params ++ modEnv2.params)
  panel.show


  override def newVoice(note: Int, detune: Float=0):Tuple2[SiGen, List[Env]] = {
    // 2(m−69)/12(440 Hz)

    val freq =  (440f * math.pow(2, ((note+detune)-69)/12f)).toFloat

    val env1 = ampEnv.env()
    env1.voiceController = Some(this)
    env1.callbackIdentifier = note
    val env2 = modEnv1.env()
    val env3 = modEnv2.env()

    var freqMod1 = modDepth1 * new SinOsc(5.02f * freq) * env2
    var freqMod2 = modDepth2 * new SinOsc(freq) * env3

    val sin1 = new SinOsc(freq + freqMod1+freqMod2)

    return (sin1 * env1, List(env1,env2,env3))

  }

}
