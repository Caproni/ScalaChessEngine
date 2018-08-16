package com.huginnmuninnresearch.chess.record

import com.huginnmuninnresearch.chess.state.Moves.Gameplay
import com.huginnmuninnresearch.chess.state.{Move, Moves}

import scala.annotation.tailrec

class MoveHistory(val id: String, val m: Moves = Moves()) {

  def addMove(move: Move): Unit = m.add(move)

  def number: Int = 1+(m.moves.length / 2)

  def whiteMove: Boolean = if (m.moves.length % 2 == 0) true else false

  def lastPieceTaken: Int = {
    val takenPieces = for (move <- m.moves) yield move.taken.isDefined
    1+(takenPieces.lastIndexOf(true)/2)
  }

  def lastPawnMove: Int = {
    val pawnMoves = for (move <- m.moves) yield move.piece.id == "Pawn"
    1+(pawnMoves.lastIndexOf(true)/2)
  }

  override def toString: String = {
    @tailrec
    def printer(moves: Gameplay, moveCount: Int = 1, output: String = ""): String = if (moves.nonEmpty) {
      printer(moves.tail, moveCount + 1, output + (if (moveCount % 2 == 0) "" else (1+moveCount/2).toString+") ") + moves.head.toString + (if (moveCount % 2 == 0) "\n" else ""))
    } else output
    s"Game: $id\n-----------------------------\n${printer(m.moves)}"
  }

}

object MoveHistory {
  def apply(id: String): MoveHistory = new MoveHistory(id)
}
