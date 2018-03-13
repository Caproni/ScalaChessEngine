package com.huginnmuninnresearch.electrical

object ACVoltage {
  import Math._
  def calcVoltage(amplitude: Double, frequency: Double)(time: Double) = amplitude*Math.sin(2*PI*frequency*time)

  def main(args: Array[String]): Unit = {
    val part = calcVoltage(1, 2)_
    println(part)
  }
}
