package com.huginnmuninnresearch.chess.state

object ChessParameters extends Enumeration {

  val Empty, Pawn, Knight, Bishop, Rook, Queen, King = Value

  val White: Value = Value("White")
  val Black: Value = Value("Black")

  final val boardSize = 8

  object PieceNotation extends Enumeration {
    val Empty: Value = Value(" ")
    val Pawn: Value = Value("P")
    val Knight: Value = Value("N")
    val Bishop: Value = Value("B")
    val Rook: Value = Value("R")
    val Queen: Value = Value("Q")
    val King: Value = Value("K")
  }

  object Squares extends Enumeration {
    val White: Value = Value(" ")
    val Black: Value = Value(".")
  }

}
