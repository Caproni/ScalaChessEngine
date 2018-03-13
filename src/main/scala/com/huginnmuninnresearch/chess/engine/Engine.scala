package com.huginnmuninnresearch.chess.engine

import com.huginnmuninnresearch.chess.pieces.Piece
import com.huginnmuninnresearch.chess.state.Board

class Engine {
  import Board._

  def bestMove(state: Board) = ???

  private def totalAvailableMoves(state: boardT): Int = {
    1
  }

  private def value(piece: Piece): Double = {
    import Engine.PieceValue._
    piece.id match {
      case "Pawn" => Pawn
      case "Knight" => Knight
      case "Bishop" => Bishop
      case "Rook" => Rook
      case "Queen" => Queen
      case "King" => King
    }
  }

}

object Engine {

  val searchDepth: Int = 5

  object PieceValue {
    val Pawn: Double = 1.0
    val Knight: Double = 3.0
    val Bishop: Double = 3.0
    val Rook: Double = 5.0
    val Queen: Double = 9.0
    val King: Double = 40.0
  }
}
