package com.huginnmuninnresearch.statistics

import scala.annotation._

object DistributionTests extends App {

  def calcKolmogorovSmirnovParameter(d1: Array[Double], d2: Array[Double], sum: Double): Double = {
    assert(d1.length == d2.length, "The two distributions do not have the same number of elements. Perhaps one should be interpolated?")
    val N: Int = d1.length
    val s1: Array[Double] = d1.sortBy(_.signum)
    val s2: Array[Double] = d2.sortBy(_.signum)
    val f: Double = calcEmpiricalDistSum(d1, d2) / d1.length
    f
  }

  @tailrec
  private def calcEmpiricalDistSum(d1: Array[Double], d2: Array[Double], sum: Int = 0): Int = {
    if ((d1.head - d2.head) > 0) calcEmpiricalDistSum(d1.tail, d2.tail, sum+1) else calcEmpiricalDistSum(d1.tail, d2.tail, sum)
  }

}
