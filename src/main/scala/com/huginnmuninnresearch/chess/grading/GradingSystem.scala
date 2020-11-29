package com.huginnmuninnresearch.chess.grading

import com.typesafe.scalalogging.LazyLogging

class GradingSystem extends LazyLogging {
  import Math._
  import com.huginnmuninnresearch.chess.state.Board._
  val k1 = 30
  val k2 = 400

  def updateGrades(result: String, whiteGrade: Int, blackGrade: Int): (Int, Int) = {

    def eloGradeFactor(whiteGrade: Int, blackGrade: Int, resultScore: Double): Int = {
      val probabilityWhiteWins: Double = 1.0 / (1.0 + pow(10, (whiteGrade - blackGrade) / k2))
      println(f"Probability white wins: $probabilityWhiteWins")
      round(k1 * (resultScore - probabilityWhiteWins)).toInt
    }

    def thisEloGradeFactor = eloGradeFactor(whiteGrade, blackGrade, _: Double)

    val scoreDelta = result match {
      case WHITE => thisEloGradeFactor(1.0)
      case BLACK => thisEloGradeFactor(0.0)
      case DRAW => thisEloGradeFactor(0.5)
      case _ => println("Check grading system inputs. Original grades returned"); 0
    }

    (whiteGrade + scoreDelta, blackGrade - scoreDelta)
  }
}
