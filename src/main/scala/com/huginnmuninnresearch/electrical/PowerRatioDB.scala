package com.huginnmuninnresearch.electrical

object PowerRatioDB {

  def calcDBFromPower(p1: Double, p2: Double): Double = 10*Math.log10(p2/p1)
  def calcDBFromAmplitude(a1: Double, a2: Double): Double = 20*Math.log10(a2/a1)

}
