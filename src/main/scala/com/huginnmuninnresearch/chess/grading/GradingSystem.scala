package com.huginnmuninnresearch.chess.grading

import com.typesafe.scalalogging.LazyLogging

class GradingSystem extends LazyLogging {
  import Math._
  import com.huginnmuninnresearch.chess.state.Board._

  def updateGrades(result: String, whiteGrade: Int, blackGrade: Int): (Int, Int) = {
    result match {
      case WHITE => (whiteGrade + 20 / (whiteGrade - blackGrade), blackGrade)
      case BLACK => (abs(whiteGrade - blackGrade) / whiteGrade, blackGrade)
      case DRAW => (abs(whiteGrade - blackGrade) / whiteGrade, blackGrade)
      case _ => println("Check grading system inputs. Original grades returned"); (whiteGrade, blackGrade)
    }
  }
}
