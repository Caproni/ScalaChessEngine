package com.huginnmuninnresearch.chess.pieces

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.state.{Board, Square}

abstract class Piece(owner: Player, location: (Int, Int)) {

  val id: String
  def coverage: Array[Square]
  def attack(board: Board): Array[Square]
  override def toString: String

}
