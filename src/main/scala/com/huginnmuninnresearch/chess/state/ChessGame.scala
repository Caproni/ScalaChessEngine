package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.engine.Engine
import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.record.MoveHistory
import com.huginnmuninnresearch.chess.state.Board._

import scala.io.StdIn

class ChessGame(white: Player, black: Player, move: String = WHITE, var result: Option[String] = None, uuid: String = java.util.UUID.randomUUID.toString) {

  import ChessGame._
  import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._

  val board: Board = new Board(white, black)
  val moveHistory: MoveHistory = MoveHistory(uuid)
  val gameEngines: Engines = engines(white, black) // engines are created for each player regardless of whether they will be used

  println(this)

  // 1W)
  var from = "d2"
  var to = "d4"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 1B)
  from = "g8"
  to = "f6"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 2W)
  from = "e2"
  to = "e4"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 2B)
  from = "f6"
  to = "e4"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 3W)
  from = "c2"
  to = "c4"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 3B)
  from = "d7"
  to = "d5"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 4W)
  from = "d1"
  to = "d3"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 4B)
  from = "h8"
  to = "g8"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 5W)
  from = "d3"
  to = "e4"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  // 5B)
  from = "d5"
  to = "c4"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  inCheck(WHITE)
  inCheck(BLACK)

  // 6W)
  from = "e4"
  to = "e7"
  move(Move(board.piece(aToI(from)).get, aToI(to), board.piece(aToI(to)), NORMAL))

  inCheck(WHITE)
  inCheck(BLACK)

//  while(result.isEmpty) { // determine whether game complete
//    val current: String = if (moveHistory.m.moves.length % 2 == 0) WHITE else BLACK // determine current turn
//    inCheck(current)
//    options(current)
//    val input = prompt(current)
//    move(input)
//    result = board.result(current, moveHistory.m.moves) // update game result
//  }
//  println(s"The winner of this game was ${result.get}")

  def prompt(current: String): Move = {
    val input: Move = if (player(current).computer) gameEngines(player(current)).bestMove(board) else {
     parse(board, current, StdIn.readLine(current + movePrompt), moveHistory.m.moves).getOrElse(prompt(current))
    }
    input
  }

  def options(current: String): Unit = {
    val moves = board.moves(current, moveHistory.m.moves)
    println(s"$current has $moves to choose from... (${moves.length} total)") // aids validation
  }

  def inCheck(current: String): Unit = {
    if (board.check(current, moveHistory.m.moves)) println(s"$current is in check!") else println(s"$current is not in check")
  }

  def move(move: Move): Unit = {
    board.move(move) // modify board state
    moveHistory.addMove(move) // add move to history
    println(this) // print new board state
  }

  def player(colour: String): Player = colour match {case WHITE => white case BLACK => black}

  override def toString: String = {
    if (moveHistory.whiteMove) {
      "\n  " + black.toString + "\n" + board.toString + "  " + white.toString + "\n" + "   (" + moveHistory.number + ")" + "\n"
    } else {
      "\n   (" + moveHistory.number + ")" + "\n  " + black.toString + "\n" + board.toString + "  " + white.toString + "\n"
    }
  }

}

object ChessGame {

  val cvcGame: String = "Would you like to view a Computer vs Computer game? "
  val colour: String = "Which colour would you like to be? (W/B) "
  val p1Name: String = "What is your name? "
  val p2Name: String = "What is the name of the second player? "
  val pvcGame: String = "Would you like to play against the computer? "
  val cName: String = "Computer"
  val movePrompt: String = ", please enter a move e.g. e4: "

  def main(args: Array[String]): Unit = {
    val g: ChessGame = new ChessGame(white = Player("Edmund"), black = Player("Richard"))
  }

  def engines(white: Player, black: Player): Engines = Map(white -> new Engine, black -> new Engine)

  type Engines = Map[Player, Engine]
}
