package com.huginnmuninnresearch.chess.record

import com.huginnmuninnresearch.chess.state.{Move, Moves}

class MoveHistory(val id: String, val m: Moves = Moves()) {

  def addMove(move: Move): Unit = m.add(move)

  def number: Int = 1+(m.moves.length / 2)

  def whiteMove: Boolean = if (m.moves.length % 2 == 0) true else false

}

object MoveHistory {
  def apply(id: String): MoveHistory = new MoveHistory(id)
}
