package com.huginnmuninnresearch.chess.state

import scala.collection.mutable.ListBuffer
import Moves._
import com.huginnmuninnresearch.chess.state.Board.Pieces

case class Moves(moves: Gameplay = ListBuffer[Move]()) {

  def copy(): Moves = {
    val copiedMoves = moves.clone()
    Moves(copiedMoves)
  }

  def add(move: Move): Unit = {
    import com.huginnmuninnresearch.chess.pieces.Piece._
    moves.append(Move(pieceInstance(move.piece), move.to, move.taken).copy)
  }

  def add(otherMoves: Moves): Unit = {
    moves.appendAll(otherMoves.moves)
  }

  def taken: Pieces = {
    (for (move <- moves; if move.taken.nonEmpty) yield move.taken.get).toArray
  }

  def taken(owner: String): Pieces = {
    taken.filter(_.owner == owner)
  }

  def last: Move = moves.last

  override def toString: String = {
    (for (move <- moves) yield move.toString).foldRight("")(_ + " " + _)
  }

}

object Moves {
  type Gameplay = ListBuffer[Move]

  def apply(moves: Gameplay = ListBuffer[Move]()): Moves = new Moves(moves)

}
