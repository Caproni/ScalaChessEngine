package com.huginnmuninnresearch.electrical

import scala.annotation.tailrec

object AddInParallel {
  def calcParallelSum(input: Double*): Double = {
    @tailrec
    def calc(total: Double, input: Seq[Double]): Double = {
      if (input.nonEmpty) calc(total+(1/input.head), input.tail) else total
    }
    1 / calc(0, input)
  }
}
