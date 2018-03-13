package com.huginnmuninnresearch.chess.state

object Chess extends Enumeration {

  val Empty, Pawn, Knight, Bishop, Rook, Queen, King = Value

  val White: Value = Value("White")
  val Black: Value = Value("Black")

  final val sideSize = 8 // length of a row or column
  final val boardSize = 64 // number of squares on a chess board

}
