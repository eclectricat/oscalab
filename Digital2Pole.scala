class Digital2Pole(input: SiGen, cutoff: SiGen, reso: SiGen) extends SiGen {

  var v0=0f
  var v1=0f
  val eps = 0.01f

  def getValue(sid: Int):Float = {

          val cut =  cutoff.getValue(sid)
          val in = input.getValue(sid)
          var fb = reso.getValue(sid)
          fb =  fb + fb/(1.0f - cut + eps);
          v0 = v0 + cut  * (in - v0 + fb * (v0 - v1))
          v1 = v1 + cut  * (v0 - v1);
          return v1
  }

}
