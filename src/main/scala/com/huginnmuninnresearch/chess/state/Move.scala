package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
import com.huginnmuninnresearch.chess.pieces.Piece
import com.huginnmuninnresearch.chess.pieces.Piece.Index
import com.huginnmuninnresearch.chess.record.MoveHistory

case class Move(piece: Piece, to: Index, taken: Option[Piece], result: String = "") {

  def valid(implicit b: Board, mH: MoveHistory): Boolean = {
    piece.legal(b, mH).contains(to)
  }

  def castling: Boolean = { // establishes whether someone is trying to castle, does not provide legality checking
    import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
    if (piece.id == "King" && !piece.moved && (to._2 == aToC("g") || to._2 == aToC("c"))) true else false
  }

  def copy: Move = Move(piece.copy, to, taken, result)

  override def toString: String = {
    (if (piece.toString.toUpperCase == "P") "" else piece.toString.toUpperCase) + piece.location.toLowerCase + " -> " + iToA(to).toLowerCase + " (" + taken.getOrElse("-").toString.toUpperCase + ") " + result
  }

}
