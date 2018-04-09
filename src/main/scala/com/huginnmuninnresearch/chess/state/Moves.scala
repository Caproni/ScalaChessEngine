package com.huginnmuninnresearch.chess.state

import scala.collection.mutable.ListBuffer
import Moves._

case class Moves(moves: Gameplay = ListBuffer[Move]()) {

  def add(move: Move): Unit = {
    moves.append(move)
  }

  def add(other: Moves): Unit = {
    moves.appendAll(other.moves)
  }

  override def toString: String = {
    (for (move <- moves) yield move.toString).foldRight("")(_.toString + " " + _)
  }

}

object Moves {
  type Gameplay = ListBuffer[Move]

  def apply(moves: Gameplay = ListBuffer[Move]()): Moves = new Moves(moves)

}
