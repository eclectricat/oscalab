

class ADSRMacro(envName: String) {

  val attack = new ConstantValue(0.2f)
  val release = new ConstantValue(1f)
  val sustain = new ConstantValue(0.5f)
  val decay = new ConstantValue(1)


  val pAttack = new ParamInfo(envName+ "-attack", 0, 2, attack)
  val pRelease = new ParamInfo(envName + "-release", 0, 3, release)
  val pSustain = new ParamInfo(envName+ "-sustain", 0, 1, sustain)
  val pDecay = new ParamInfo(envName + "-decay", 0, 3, decay)

  def env() = new Env(attack.getValue(0), decay.getValue(0), sustain.getValue(0), release.getValue(0), None, 0)

  val params = List(pAttack, pDecay, pSustain, pRelease)
}
