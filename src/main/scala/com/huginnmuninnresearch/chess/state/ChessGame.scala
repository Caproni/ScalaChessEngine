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

  // Gorkem Unsal

//  move(parse(board, WHITE, "e2->e4", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "e7->e6", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "f2->f3", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "d7->d5", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "d2->d4", moveHistory.m.moves).get)

  // Alexander Job Ward

//  move(parse(board, WHITE, "e2->e4", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "e7->e5", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "d2->d4", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "d7->d5", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "d4->e5", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "d5->e4", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "d1->d8", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "e8->d8", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "e5->e6", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "c8->e6", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "f2->f3", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "f7->f5", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "f3->f4", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "b8->c6", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "c2->c3", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "d8->d7", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "c1->e3", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "a8->d8", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "e3->d4", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "d7->c8", moveHistory.m.moves).get)
//  move(parse(board, WHITE, "e1->d1", moveHistory.m.moves).get)
//  move(parse(board, BLACK, "d8->d4", moveHistory.m.moves).get)


  while(result.isEmpty) { // determine whether game complete
    val current: String = if (moveHistory.m.moves.length % 2 == 0) WHITE else BLACK // determine current turn
//    inCheck(current)
//    options(current)
    val input = prompt(current)
    move(input)
    result = board.result(current, moveHistory) // update game result
  }
  println(s"The winner of this game was ${result.get}")

  def prompt(current: String): Move = {
    val input: Move = if (player(current).computer) {
      gameEngines(player(current)).bestMove(current, board, moveHistory)
    } else {
     parse(board, current, StdIn.readLine(current + movePrompt), moveHistory).getOrElse(prompt(current))
    }
    input
  }

  def options(current: String): Unit = {
    val moves = board.moves(current, moveHistory)
    println(s"$current has $moves to choose from... (${moves.length} total)") // aids validation
  }

  def inCheck(current: String): Unit = {
    if (board.check(current, moveHistory)) println(s"$current is in check!") else println(s"$current is not in check")
  }

  def move(move: Move): Unit = {
    moveHistory.addMove(move.copy) // add move to history
    board.move(move.copy, moveHistory) // modify board state
    println(this) // print new board state
  }

  def player(colour: String): Player = colour match {case WHITE => white case BLACK => black}

  override def toString: String = {
    if (moveHistory.whiteMove) {
      s"Game: $uuid\n  ${black.toString}\n${board.toString}  ${white.toString}\n   (${moveHistory.number})\n"
    } else {
      s"Game: $uuid\n   (${moveHistory.number})\n  ${black.toString}\n${board.toString}  ${white.toString}\n"
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
    val computer: Boolean = true
    val g: ChessGame = new ChessGame(
      white = Player("Edmund Bennett", computer),
      black = Player("Alexander Ward", computer)
    )
  }

  def engines(white: Player, black: Player): Engines = Map(white -> new Engine, black -> new Engine)

  type Engines = Map[Player, Engine]
}
