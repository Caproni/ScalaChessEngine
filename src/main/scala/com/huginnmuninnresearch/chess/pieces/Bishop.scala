package com.huginnmuninnresearch.chess.pieces

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.state.{Board, Square}

class Bishop(owner: Player, var location: (Int, Int)) extends Piece(owner, location) {

  override val id: String = "Bishop"
  override def coverage: Array[Square] = ???
  override def attack(board: Board): Array[Square] = ???
}
