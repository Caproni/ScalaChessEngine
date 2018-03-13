package com.huginnmuninnresearch.chess.pieces

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.state.Square

class Queen(owner: Player, var location: (Int, Int)) extends Piece(owner, location) {

  override val id: String = "Queen"
  override def coverage: Array[Square] = ???
  override def attack: Array[Square] = ???
}
