package com.huginnmuninnresearch.chess.state

import Move._
import com.huginnmuninnresearch.chess.pieces.Piece

case class Move(From: (Int, Int), To: (Int, Int), piece: Piece, taken: Option[Piece], result: String = normal) {

}

object Move {
  final val normal = "."
  final val check = "+"
  final val checkmate = "++"
}
