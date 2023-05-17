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
  		/**y1 = (g * tanh( x0 ) + iceq1 ) * gDiv;
  		y2 = (g * tanh( y1 ) + iceq2 ) * gDiv;
  		y3 = (g * tanh( y2 ) + iceq3 ) * gDiv;
  		val y4:Double = (g * tanh( y3 ) + iceq4 ) * gDiv;*/

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

			y1 = (g * tanh(x0)  + iceq1 ) * gDiv;
  		y2 = (g * y1  + iceq2 ) * gDiv;
  		y3 = (g * y2  + iceq3 ) * gDiv;
  		val y4:Double = (g * y3 + iceq4 ) * gDiv;

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
