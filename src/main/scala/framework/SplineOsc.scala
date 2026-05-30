package framework



class SplineOsc(val freq: SiGen, var controlpoints:List[Tuple2[Float, Float]]) extends CachedSiGen {

  val nbactpoints = controlpoints.length

  // because it is cyclic, copy the same points left and right to get the border conditions right
  val addFront = (controlpoints.last._1 -1 , controlpoints.last._2)
  val addEnd1 = (controlpoints(0)._1 + 1, controlpoints(0)._2)
  val addEnd2 = (controlpoints(1)._1 + 1, controlpoints(1)._2)
  controlpoints = addFront :: controlpoints ++ List(addEnd1, addEnd2)


  val startPhase = 2
  //t = linspace(startPhase,nbactpoints+startPhase,150)
  val nbSteps = 150
  val deltaX = nbactpoints / nbSteps.toFloat

  var splinePhase: Float = startPhase

  var oscPhase:Float = 0

  // spline points
  var x:Float = 0
  var y:Float = 0

  override def calculateNext(sid: Int): Float = {

    val phaseDelta = freq.getValue(sid)/GlobalConfig.sampleRate

    // find the next datapoint on the spline
    while (x < oscPhase) {

      // move forward in the spline, until the predicted X position is larger than the one we need
      splinePhase = splinePhase + deltaX
      //if (splinePhase > startPhase + nbactpoints)
        //splinePhase -= nbactpoints // -=1


      x=0;y=0;
      // loop though controlpoints and sum them, weighted by basis fuction
      for (cpi<- 0 until controlpoints.length) {
        val bf = basis_f(splinePhase - cpi)
        x += bf * controlpoints(cpi)._1
        y += bf * controlpoints(cpi)._2
      }
    }

    oscPhase += phaseDelta
    if (oscPhase > 1) {
      oscPhase -= 1
      splinePhase -= nbactpoints
      x=0
    }

    return y

  }

  def basis_f(u: Float): Float = {
   if (u < 0) return 0
   if (u < 1) return 0.5f * u * u
   if (u < 2) return 0.5f * (-2 * u * u + 6 * u - 3)
   if (u < 3) return 0.5f * (3-u) * ( 3-u )
   return 0
  }


}
