package framework

import scala.math._


class CircuitModelerTwin(input: SiGen, cutoff: SiGen, resonance: SiGen) extends CachedSiGen {


	var factor = 5000

	var capacitor1_C: Double = 2.2e-7 * factor
	var capacitor2_C: Double = 2.2e-7 * factor
	//var capacitor_C: Double = 100e-7 * factor
	//var capacitor3_C: Double = 1e-6 * factor

	var r_Resistor_R: Double = 10e6
	var r_Resistor1_R: Double = 1e3 // resonance. orig: 100e3, smaller, e.g. 1e3, more reso
	var r_Resistor2_R: Double = 3e6
	var r_Resistor3_R: Double = 100e3
	var r_Resistor4_R: Double = 1.5e6
	var r_Resistor5_R: Double = 1.5e6
	var r_Resistor6_R: Double = .5e3 // cutoff : 0.1e3 to 10e3     orig: 5e3
	var capacitor1_v = 0.0
	var capacitor3_state = 0.0
	var signalVoltage_v = 0.0
	var capacitor_v = 0.0
	var capacitor2_v = 0.0
	var capacitor1_state = 0.0
	var capacitor_state = 0.0
	var capacitor2_state = 0.0
	var voltageSensor_v = 0.0
	var capacitor3_v = 0.0


  var out:Double = 0

	def calculateNext(sid: Int):Float = {
    val in = input.getValue(sid)
    out = in

    //XX_PARAMS
    val cut: Float =  min(1, cutoff.getValue(sid))
    var targetFreq = 0.001f * pow(2,9*cut).toFloat // exponential mapping from cutoff param to f_c/f_s
    val resistor = 1 / (2 * Pi * targetFreq * 1)

		// cutoff
		val resX = 1 / (2 * Pi * targetFreq * capacitor1_C * 50)
		r_Resistor6_R = resX
		//r_Resistor6_R = 0.05e3 + (1-cut) * (10e3-0.1e3)

    // reso
		val reso: Float =  min(1, resonance.getValue(sid))
		r_Resistor1_R = 0.1e3 + (1-reso) * (20e3 - 0.1e3)

    signalVoltage_v = in
// TODO: update any params/constants as needed, e.g. cutoff etc.
capacitor1_v = (-2*capacitor1_state*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - 2*capacitor1_state*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - capacitor1_state*r_Resistor1_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R - capacitor1_state*r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R - capacitor1_state*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v + capacitor2_state*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*signalVoltage_v)/(4*capacitor1_C*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 4*capacitor1_C*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R + 2*capacitor2_C*r_Resistor1_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + r_Resistor1_R*r_Resistor4_R*r_Resistor_R + r_Resistor1_R*r_Resistor6_R*r_Resistor_R + r_Resistor2_R*r_Resistor6_R*r_Resistor_R + r_Resistor4_R*r_Resistor5_R*r_Resistor_R + r_Resistor4_R*r_Resistor6_R*r_Resistor_R + r_Resistor5_R*r_Resistor6_R*r_Resistor_R)
capacitor2_v = (-2*capacitor1_C*capacitor2_state*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - 2*capacitor1_C*capacitor2_state*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v - capacitor1_state*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R + capacitor1_state*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - capacitor2_state*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R - capacitor2_state*r_Resistor1_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - capacitor2_state*r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - capacitor2_state*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*signalVoltage_v + r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v)/(4*capacitor1_C*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 4*capacitor1_C*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R + 2*capacitor2_C*r_Resistor1_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + r_Resistor1_R*r_Resistor4_R*r_Resistor_R + r_Resistor1_R*r_Resistor6_R*r_Resistor_R + r_Resistor2_R*r_Resistor6_R*r_Resistor_R + r_Resistor4_R*r_Resistor5_R*r_Resistor_R + r_Resistor4_R*r_Resistor6_R*r_Resistor_R + r_Resistor5_R*r_Resistor6_R*r_Resistor_R)
voltageSensor_v = (-4*capacitor1_C*capacitor2_C*r_Resistor1_R*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v + 2*capacitor1_C*capacitor2_state*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - 2*capacitor1_C*r_Resistor1_R*r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*signalVoltage_v - 2*capacitor1_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v + 2*capacitor1_state*capacitor2_C*r_Resistor1_R*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R + 2*capacitor1_state*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + capacitor1_state*r_Resistor1_R*r_Resistor2_R*r_Resistor4_R*r_Resistor_R + capacitor1_state*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R + capacitor1_state*r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R - 2*capacitor2_C*r_Resistor1_R*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*signalVoltage_v - 2*capacitor2_C*r_Resistor1_R*r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v - 2*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v + capacitor2_state*r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R - r_Resistor1_R*r_Resistor2_R*r_Resistor4_R*signalVoltage_v - r_Resistor1_R*r_Resistor2_R*r_Resistor6_R*signalVoltage_v - r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*signalVoltage_v - r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*signalVoltage_v - r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*signalVoltage_v)/(4*capacitor1_C*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 4*capacitor1_C*capacitor2_C*r_Resistor2_R*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor1_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor2_R*r_Resistor4_R*r_Resistor6_R*r_Resistor_R + 2*capacitor1_C*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor1_R*r_Resistor4_R*r_Resistor5_R*r_Resistor_R + 2*capacitor2_C*r_Resistor1_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor2_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + 2*capacitor2_C*r_Resistor4_R*r_Resistor5_R*r_Resistor6_R*r_Resistor_R + r_Resistor1_R*r_Resistor4_R*r_Resistor_R + r_Resistor1_R*r_Resistor6_R*r_Resistor_R + r_Resistor2_R*r_Resistor6_R*r_Resistor_R + r_Resistor4_R*r_Resistor5_R*r_Resistor_R + r_Resistor4_R*r_Resistor6_R*r_Resistor_R + r_Resistor5_R*r_Resistor6_R*r_Resistor_R)
out = voltageSensor_v
// update states
capacitor1_state = -2 * 2 * capacitor1_C * capacitor1_v - capacitor1_state
capacitor2_state = -2 * 2 * capacitor2_C * capacitor2_v - capacitor2_state

		return out.toFloat;
	}

}
