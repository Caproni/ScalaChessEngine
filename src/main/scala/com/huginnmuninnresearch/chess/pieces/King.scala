package com.huginnmuninnresearch.chess.pieces

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.state.{Board, Square}

class King(owner: Player, var location: (Int, Int)) extends Piece(owner, location) {

  override val id: String = "King"
  override def coverage: Array[Square] = { // accessible squares not accounting for other pieces on the board
    location match {
      case location._1 == 0 => // on an edge
    }
  }
  override def attack(board: Board): Array[Square] = ??? // accessible squares accounting for other pieces on the board
}
