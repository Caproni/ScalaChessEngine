package com.huginnmuninnresearch.electrical

object RCChargingCircuit {

  def calcVoltage(amplitude: Double, resistance: Double, capacitance: Double)(time: Double): Double = {
    amplitude*Math.exp(-time/calcTimeConstant(resistance,capacitance))
  }
  def calcTimeConstant(resistance: Double, capacitance: Double): Double = resistance*capacitance

  def calcVoltageOut(batteryVoltage: Double, amplitude: Double, resistance: Double, capacitance: Double)(time: Double): Double = {
    batteryVoltage + calcVoltage(amplitude, resistance, capacitance)(time)
  }

  def calcTimeToVoltage(timeConstant: Double, finalVoltage: Double, currentVoltage: Double): Double = {
    timeConstant * Math.log(finalVoltage / (finalVoltage - currentVoltage))
  }



}
