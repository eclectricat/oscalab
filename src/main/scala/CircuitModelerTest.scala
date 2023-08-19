import scala.math._


class CircuitModelerTest(input: SiGen, cutoff: SiGen) extends CachedSiGen {

	var r_Resistor2_R: Double = 30
var signalVoltage_v = 0.0
var voltageSensor_v = 0.0
var r_Resistor1_R: Double = 0
var capacitor_C: Double = 1
var r_Resistor_R: Double = 0
var capacitor_state = 0.0
var capacitor_v = 0.0
var r_Resistor3_R: Double = 100
var capacitor1_v = 0.0
var capacitor1_state = 0.0
var capacitor1_C: Double = 1

  var out:Double = 0

	def calculateNext(sid: Int):Float = {
    val in = input.getValue(sid)
    out = in

    //XX_PARAMS
    val cut: Float =  min(1, cutoff.getValue(sid))
    var targetFreq = 0.001f * pow(2,9*cut).toFloat // exponential mapping from cutoff param to f_c/f_s
    val resistor = 1 / (2 * Pi * targetFreq * 1)

    signalVoltage_v = in
// TODO: update any params/constants as needed, e.g. cutoff etc.

r_Resistor1_R = resistor
r_Resistor_R = resistor

capacitor1_v = (-2*capacitor1_state*capacitor_C*r_Resistor1_R*r_Resistor3_R*r_Resistor_R - capacitor1_state*r_Resistor1_R*r_Resistor3_R - capacitor_state*r_Resistor3_R*r_Resistor_R + r_Resistor3_R*signalVoltage_v)/(4*capacitor1_C*capacitor_C*r_Resistor1_R*r_Resistor3_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor3_R - 2*capacitor_C*r_Resistor2_R*r_Resistor_R + r_Resistor3_R)
capacitor_v = (-2*capacitor1_C*capacitor_state*r_Resistor1_R*r_Resistor3_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor3_R*signalVoltage_v + capacitor1_state*r_Resistor1_R*r_Resistor2_R + capacitor1_state*r_Resistor1_R*r_Resistor3_R + capacitor_state*r_Resistor2_R*r_Resistor_R - r_Resistor2_R*signalVoltage_v)/(4*capacitor1_C*capacitor_C*r_Resistor1_R*r_Resistor3_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor3_R - 2*capacitor_C*r_Resistor2_R*r_Resistor_R + r_Resistor3_R)
voltageSensor_v = (-2*capacitor1_state*capacitor_C*r_Resistor1_R*r_Resistor3_R*r_Resistor_R - capacitor1_state*r_Resistor1_R*r_Resistor3_R - capacitor_state*r_Resistor3_R*r_Resistor_R + r_Resistor3_R*signalVoltage_v)/(4*capacitor1_C*capacitor_C*r_Resistor1_R*r_Resistor3_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor3_R - 2*capacitor_C*r_Resistor2_R*r_Resistor_R + r_Resistor3_R)
out = voltageSensor_v
// update states
capacitor_state = -2 * 2 * capacitor_C * capacitor_v - capacitor_state
capacitor1_state = -2 * 2 * capacitor1_C * capacitor1_v - capacitor1_state

		return out.toFloat;
	}

}
