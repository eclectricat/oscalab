package framework

import scala.math._


// from: https://www.kvraudio.com/forum/viewtopic.php?t=412944&sid=df1651b1f969dbb46e747060c0fd9b35

class Digital4Pole(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  //var v0=0f
  //var v1=0f
  //val eps = 0.01f

	var iceq1:Double = 0	// states in form
	var iceq2:Double= 0	// of current
	var iceq3:Double= 0	// equivalents
	var iceq4:Double= 0	// of capacitors

	var y4:Double= 0;		// delayed feedback

	def calculateNext(sid: Int):Float = {
    val in = input.getValue(sid)
	  val cut =  cutoff.getValue(sid) * 0.5
		//val g = tan( Pi * cut/GlobalConfig.sampleRate );
    val g = tan( Pi * cut);
	  val gDiv = 1.0f/(1.0 + g);
		val x0 = (in - (reso.getValue(sid)* 4) * y4 );

		/**val y1 = (g * tanh( x0 ) + iceq1 ) * gDiv;
		val y2 = (g * tanh( y1 ) + iceq2 ) * gDiv;
		val y3 = (g * tanh( y2 ) + iceq3 ) * gDiv;
		y4 = (g * tanh( y3 ) + iceq4 ) * gDiv;*/


    val y1 = (g * x0  + iceq1 ) * gDiv;
		val y2 = (g * y1  + iceq2 ) * gDiv;
		val y3 = (g * y2  + iceq3 ) * gDiv;
		y4 = (g *  tanh( y3 )  + iceq4 ) * gDiv;
		//y4 = tanh ( (g * y3  + iceq4 ) * gDiv );

		iceq1 = 2*y1 - iceq1;
		iceq2 = 2*y2 - iceq2;
		iceq3 = 2*y3 - iceq3;
		iceq4 = 2*y4 - iceq4;

		return y4.toFloat;
	}

}

class Digital4PoleZDF(input: SiGen, cutoff: SiGen, reso: SiGen) extends CachedSiGen {

  //var v0=0f
  //var v1=0f
  //val eps = 0.01f

	var iceq1:Double = 0	// states in form
	var iceq2:Double= 0	// of current
	var iceq3:Double= 0	// equivalents
	var iceq4:Double= 0	// of capacitors

	var y4:Double= 0;		// delayed feedback

	def calculateNext(sid: Int):Float = {
    val in = input.getValue(sid)
	  val cut =  cutoff.getValue(sid) * 0.5
    val resonance = 4*reso.getValue(sid)
		//val g = tan( Pi * cut/GlobalConfig.sampleRate );
    val g = tan( Pi * cut);
	  val gDiv = 1.0f/(1.0 + g);


    var y1,y2,y3:Double = 0

    def runFilter(in:Float, outEstimate:Double ):Double = {



			val x0 = (in - resonance * outEstimate );

      // tan in every input
  		y1 = (g * tanh( x0 ) + iceq1 ) * gDiv;
  		y2 = (g * tanh( y1 ) + iceq2 ) * gDiv;
  		y3 = (g * tanh( y2 ) + iceq3 ) * gDiv;
  		val y4:Double = (g * tanh( y3 ) + iceq4 ) * gDiv;

      // tan only in last stage
      /**y1 = (g * x0  + iceq1 ) * gDiv;
  		y2 = (g * y1  + iceq2 ) * gDiv;
  		y3 = (g * y2  + iceq3 ) * gDiv;
  		val y4:Double = (g * tanh(y3) + iceq4 ) * gDiv;*/
			//val y4:Double = (g * y3 + iceq4 ) * gDiv;
			//val y4:Double  = tanh ( (g * y3  + iceq4 ) * gDiv );


			// tan only in feedback experiment
      /**val x0 = (in - resonance * tanh(scala.math.pow(outEstimate, 1.1)));
			y1 = (g * x0  + iceq1 ) * gDiv;
  		y2 = (g * y1  + iceq2 ) * gDiv;
  		y3 = (g * y2  + iceq3 ) * gDiv;
  		val y4:Double = (g * y3 + iceq4 ) * gDiv;*/

      // tan only at mix in point
			/**y1 = (g * tanh(x0)  + iceq1 ) * gDiv;
  		y2 = (g * y1  + iceq2 ) * gDiv;
  		y3 = (g * y2  + iceq3 ) * gDiv;
  		val y4:Double = (g * y3 + iceq4 ) * gDiv;*/

  		return (y4 - outEstimate).toFloat // returns the error
    }

		var EstimateLow = -2f;
		var errorLow = runFilter( in, EstimateLow );

		var EstimateHigh = 2f;
		var errorHigh = runFilter( in, EstimateHigh );

		var outEstimate:Float = 0f;

    var counter = 0
    while( (abs( errorLow - errorHigh ) > 0.0001f) && (counter < 100) )
		{
      counter += 1
			outEstimate = ( EstimateHigh + EstimateLow ) * 0.5f;
      //System.out.println("out estimate:"+ outEstimate)


			val newError = runFilter( in, outEstimate);

      //System.out.println("new error:"+ newError)

			if( signum( newError ) == signum( errorLow ) )
			{
				EstimateLow = outEstimate;
				errorLow = newError;

			}
			else
			{
				EstimateHigh = outEstimate;
				errorHigh = newError;
			}

		}

    //System.out.println("counter "+ counter)

		iceq1 = 2*y1 - iceq1;
		iceq2 = 2*y2 - iceq2;
		iceq3 = 2*y3 - iceq3;
		iceq4 = 2*outEstimate - iceq4;

		return outEstimate.toFloat;
	}

}

// try 4 pole filter with tan(x-y) nonlinearity and fixed point iteration
class Digital4PoleFP(input: SiGen, cutoff: SiGen, reso: SiGen, ftype:String="ota4p") extends CachedSiGen {

  //var v0=0f
  //var v1=0f
  //val eps = 0.01f

	var iceq1:Double = 0	// states in form
	var iceq2:Double= 0	// of current
	var iceq3:Double= 0	// equivalents
	var iceq4:Double= 0	// of capacitors

	var hp_feedback: Double = 0

  var y1,y2,y3:Double = 0
	var y4:Double= 0;		// to store the output from the last iteration

  // statistics
	var totalIters = 0;
	var totalError = 0f;

  // oversampling
	var inTminus1 = 0f; // previour input sample, needed for linear interpolation in oversampling
	val osFactor = 4 // 4

	val samples = new Array[Float](4)

	def calculateNext(sid: Int):Float = {
    val in = input.getValue(sid)
	  val cut =  min(0.95f, cutoff.getValue(sid)) * 0.5

    val resonance = 4*reso.getValue(sid)

		// need to check where this formula comes from...
		//val g = tan( Pi * cut/GlobalConfig.sampleRate );
    //var g = tan( Pi * cut);
		//var g  = cut / osFactor
		val targetFreq = 0.001f * pow(2,9*cut * 2).toFloat
		var g = tan(Pi * targetFreq / osFactor)

	  //val gDiv = 1.0f/(1.0 + g);

		//val ftype = "skf"

    def runFilter(in:Float, outEstimate:Double ):Double = {

			ftype match {
				case "ota4p" => {
					val x0 = (in - resonance * outEstimate );
					y1 = g * fast_tanhf_rat(x0 - y1) + iceq1
					y2 = g * fast_tanhf_rat(y1 - y2) + iceq2
					y3 = g * fast_tanhf_rat(y2 - y3) + iceq3
					y4 = g * fast_tanhf_rat(y3 - y4) + iceq4
					return (y4 - outEstimate).toFloat // returns the error
				}

				case "ota4pinverting" => {
					val x0 = (in - resonance * outEstimate );
					y1 = g * fast_tanhf_rat(-x0 - y1) + iceq1
					y2 = g * fast_tanhf_rat(-y1 - y2) + iceq2
					y3 = g * fast_tanhf_rat(-y2 - y3) + iceq3
					y4 = g * fast_tanhf_rat(-y3 - y4) + iceq4
					return (y4 - outEstimate).toFloat // returns the error
				}

				case "linear4p" => {
					val x0 = (in - tanh(resonance * outEstimate) ); // 1 nonlinearity so it doesn't explode
					y1 = g * (x0 - y1) + iceq1
					y2 = g * (y1 - y2) + iceq2
					y3 = g * (y2 - y3) + iceq3
					y4 = g * (y3 - y4) + iceq4
					return (y4 - outEstimate).toFloat
				}

				case "skf" => { // is a 2 pole, I know
					//val feedback = fast_tanhf_rat(0.5 * resonance * outEstimate)
					val feedback = 2 * asinh(0.25 * resonance * outEstimate)
					y1 = feedback + g * fast_tanhf_rat(in - y1) + iceq1
					y2 = g * fast_tanhf_rat(y1 - y2) + iceq2
					y4 = y2 // because the surrounding logic assumes that y4 is the result

          hp_feedback = 0.5 * resonance * y2 // for the state update

					return (y2 - outEstimate).toFloat
				}

				case _ => None
			}

			return 0f

    }

		// oversampling
		for (oi <- 1 to (osFactor)) {

			val oversampledInput = inTminus1 + (oi/osFactor.toFloat) * (in - inTminus1)

			var outEstimate:Float = y4.toFloat; // start with output from last iteration

			var error = runFilter(oversampledInput, outEstimate)

			var counter = 0
			while( (abs(error) > 0.0001f) && (counter < 100) )
			{
				counter += 1
				outEstimate = y4.toFloat;

				error = runFilter(oversampledInput, outEstimate)
				//System.out.println("Iteration " + counter + ", error " + error)
			}

			totalIters += counter
			totalError += abs(error).toFloat

			if((sid % 40000 == 0) && (oi == 1)) {
				System.out.println("avg iters: "+ totalIters / 40000.0)
				System.out.println("avg error: "+ totalError / 40000.0)
				totalIters = 0
				totalError = 0
			}

			//System.out.println("counter "+ counter)
			//System.out.println("error "+ error)

		//	iceq1 = 2*y1 - iceq1;
		// in case of the feedback via the HP input

		  iceq1 = 2* (y1 - hp_feedback) - iceq1
			iceq2 = 2*y2 - iceq2;
			iceq3 = 2*y3 - iceq3;
			iceq4 = 2*y4 - iceq4;

			samples(oi-1) = y4.toFloat

		} // oversampling

		inTminus1 = in // somehow it sounds better without this

    // todo: proper downsampling from oversampled representation,
		//return y4.toFloat; // for now just take last value
		return samples.toList.sum / osFactor // poor mans downsampling
	}

  // from :  https://stackoverflow.com/questions/73770905/best-non-trigonometric-floating-point-approximation-of-tanhx-in-10-instruction
	def fast_tanhf_rat(xx:Double):Float = {
		  val x = xx.toFloat
	    val n0 = -8.73291016e-1f; // -0x1.bf2000p-1
	    val n1 = -2.76107788e-2f; // -0x1.c46000p-6
	    val d0 =  2.79589844e+0f; //  0x1.65e000p+1
	    val x2 = x * x;
	    //val num = fmaf (n0, x2, n1);
			val num = n0 * x2 + n1
	    val den = x2 + d0
	    val quot = num / den
	    var res = quot * x + x
	    res = min(max (res, -1.0f), 1.0f)
	    return res.toFloat;
	}

  // http://www.java2s.com/example/java-utility-method/asinh/asinh-double-a-8e35c.html
	def asinh(aa:Double): Float =  {
        var sign: Double = 0
				var a = aa
        // check the sign bit of the raw representation to handle -0
        if (java.lang.Double.doubleToRawLongBits(a) < 0) {
            a = abs(a)
            sign = -1.0d
        } else {
            sign = 1.0d
        }

        return (sign * log(sqrt(a * a + 1.0d) + a)).toFloat;
    }


}





/**
class filterZDF
{
public:

	... stuff ...

	float iceq1;	// states in form
	float iceq2;	// of current
	float iceq3;	// equivalents
	float iceq4;	// of capacitors

	float runFilter( float in, float outEstimate, float &y1, float &y2, float &y3 )
	{
		float g = tan( PI * cutoff/samplerate );
		float gDiv = 1.f/(1.0 + g);

		float x0 = (in - resonance * outEstimate );

		y1 = (g * tanh( x0 ) + iceq1 ) * gDiv;
		y2 = (g * tanh( y1 ) + iceq2 ) * gDiv;
		y3 = (g * tanh( y2 ) + iceq3 ) * gDiv;
		float y4 = (g * tanh( y3 ) + iceq4 ) * gDiv;

		return y4 - outEstimate; // returns the error
	}

	float tick( float in )
	{
		float y1, y2, y3;

		float EstimateLow = -2.f;
		float errorLow = runFilter( in, EstimateLow, y1, y2, y3 );

		float EstimateHigh = 2.f;
		float errorHigh = runFilter( in, EstimateHigh, y1, y2, y3 );

		float outEstimate = 0.f;

		while( fabs( errorLow - errorHigh ) > 0.0001f )
		{
			outEstimate = ( EstimateHigh - EstimateLow ) * 0.5f;

			float newError = runFilter( in, outEstimate, y1, y2, y3 );

			if( sign( newError ) == sign( errorLow ) )
			{
				EstimateLow = outEstimate;
				errorLow = newError;

			}
			else
			{
				EstimateHigh = outEstimate;
				errorHigh = newError;
			}
		}

		iceq1 = 2*y1 - iceq1;
		iceq2 = 2*y2 - iceq2;
		iceq3 = 2*y3 - iceq3;
		iceq4 = 2*outEstimate - iceq4;

		return outEstimate;
	}
};
*/
