package com.huginnmuninnresearch.electrical

object VoltageDivider {

  def calcCurrentOpenCircuit(Vin: Double, r1: Double, r2: Double): Double = Vin / (r1+r2)
  def calcVoltageOut(Vin: Double, r1: Double, r2: Double): Double = calcCurrentOpenCircuit(Vin, r1, r2) * r2

}
