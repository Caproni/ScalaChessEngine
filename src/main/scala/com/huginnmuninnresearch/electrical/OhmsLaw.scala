package com.huginnmuninnresearch.electrical

object OhmsLaw {
  def calcV(current: Double, resistance: Double): Double = current * resistance
  def calcI(voltage: Double, resistance: Double): Double = voltage / resistance
  def calcR(voltage: Double, current: Double): Double = voltage / current
}
