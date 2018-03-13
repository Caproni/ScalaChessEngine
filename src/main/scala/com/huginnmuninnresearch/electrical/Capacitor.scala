package com.huginnmuninnresearch.electrical

object Capacitor {

  def calcCharge(capacitance: Double, voltage: Double): Double = capacitance * voltage
  def calcCapacitance(charge: Double, voltage: Double): Double = charge / voltage
  def calcVoltage(charge: Double, capacitance: Double): Double = charge / capacitance
  def calcCurrent(capacitance: Double, deltavoltage: Double, deltaTime: Double): Double = capacitance*deltavoltage/deltaTime
  def calcEnergy(capacitance: Double, voltage: Double): Double = 0.5*capacitance*voltage*voltage


}
