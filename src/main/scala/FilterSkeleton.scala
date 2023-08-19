import scala.math._


class FilterSkeleton(input: SiGen, cutoff: SiGen) extends CachedSiGen {

	//XX_DECLARATIONS

  var out:Double = 0

	def calculateNext(sid: Int):Float = {
    val in = input.getValue(sid)
    out = in

    //XX_PARAMS
    val cut: Float =  min(1, cutoff.getValue(sid))
    var targetFreq = 0.001f * pow(2,9*cut).toFloat // exponential mapping from cutoff param to f_c/f_s
    val resistor = 1 / (2 * Pi * targetFreq * 1)

    //XX_PROCESSING_PLACEHOLDER

		return out.toFloat;
	}

}
