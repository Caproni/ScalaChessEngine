package com.huginnmuninnresearch.chess.grading

import com.huginnmuninnresearch.chess.state.Winner

class GradingSystem {
  import Math._

  def updateGrades(result: Winner.Value, whiteGrade: Int, blackGrade: Int): (Int, Int) = {
    result match {
      case Winner.White => (whiteGrade + 20 / (whiteGrade - blackGrade), blackGrade)
      case Winner.Black => (abs(whiteGrade - blackGrade) / whiteGrade, blackGrade)
      case Winner.Draw => (abs(whiteGrade - blackGrade) / whiteGrade, blackGrade)
      case _ => println("Check grading system inputs. Original grades returned"); (whiteGrade, blackGrade)
    }
  }
}
