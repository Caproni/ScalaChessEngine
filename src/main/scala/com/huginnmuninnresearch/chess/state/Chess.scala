package com.huginnmuninnresearch.chess.state

object Chess extends Enumeration {

  val Empty, Pawn, Knight, Bishop, Rook, Queen, King = Value

  final val White: Value = Value("White")
  final val Black: Value = Value("Black")

}
