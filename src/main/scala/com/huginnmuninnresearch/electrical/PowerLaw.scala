package com.huginnmuninnresearch.electrical

object PowerLaw {
  def calcPower(current: Double, voltage: Double): Double = current * voltage
  def calcCurrent(power: Double, voltage: Double): Double = power / voltage
  def calcVoltage(power: Double, current: Double): Double = power / current
}
