package com.huginnmuninnresearch.chess.grading

import com.huginnmuninnresearch.chess.state.Board

class ChessGame(white: Player, black: Player, result: Option[Winner.Value] = None) {
  val state: Board = new Board(white, black)
  result.getOrElse("Game on!") match {
    case s: String => println(s); val board = new Board(white, black); println(board)
    case Winner.White => println("The winner of this game was White")
    case Winner.Black => println("The winner of this game was Black")
    case Winner.Draw => println("This game was a Draw")
  }
}

object ChessGame {
  def main(args: Array[String]): Unit = {
    new ChessGame(Player("Edmund"), Player("Computer"))
  }
}
