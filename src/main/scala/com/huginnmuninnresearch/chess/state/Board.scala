package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.grading.Player

import scala.annotation.tailrec

class Board(white: Player, black: Player, var board: Array[Array[Square]]) {
  import Board._
  def this(white: Player, black: Player) = this(white, black, {
    val empty = Array.ofDim[Square](ChessParameters.boardSize, ChessParameters.boardSize)
    for (row <- 0 until ChessParameters.boardSize; col <- 0 until ChessParameters.boardSize) {
      empty(row)(col) = new Square(row, col)
    }
    empty
  })

  private def populateMajorPieces(board: boardT, row: Int, owner: ChessParameters.Value): boardT = {
    for (col <- 0 until ChessParameters.boardSize) {
      col match {
        case 0 | 7 => board(row)(col) = Square(row, col, ChessParameters.Rook, Some(owner))
        case 1 | 6 => board(row)(col) = Square(row, col, ChessParameters.Knight, Some(owner))
        case 2 | 5 => board(row)(col) = Square(row, col, ChessParameters.Bishop, Some(owner))
        case 4 => board(row)(col) = Square(row, col, ChessParameters.Queen, Some(owner))
        case 3 => board(row)(col) = Square(row, col, ChessParameters.King, Some(owner))
      }
    }
    board
  }

  private def initialiseBoard: boardT = {
    for (row <- 0 until ChessParameters.boardSize) {
      row match {
        case 0 => populateMajorPieces(board, row, ChessParameters.White)
        case 7 => populateMajorPieces(board, row, ChessParameters.Black)
        case 1 => for (col <- 0 until ChessParameters.boardSize) {board(row)(col) = Square(row, col, ChessParameters.Pawn, Some(ChessParameters.White))}
        case 6 => for (col <- 0 until ChessParameters.boardSize) {board(row)(col) = Square(row, col, ChessParameters.Pawn, Some(ChessParameters.Black))}
        case _ => ()
      }
    }
    board
  }

  def movePiece(from: (Int, Int), to: (Int, Int)): Unit = {
    board(from._1)(from._2) = new Square(from._1, from._2)

  }

  board = initialiseBoard

  override def toString: String = {
    val slice: String = "----------\n"
    @tailrec
    def constructStringOuter(board: boardT, outerResult: String = ""): String = {
      if (board.nonEmpty) {
        @tailrec
        def constructStringInner(line: Array[Square], innerResult: String = ""): String = {
          if (line.nonEmpty) constructStringInner(line.tail, innerResult+line.head.toString) else innerResult
        }
        constructStringOuter(board.tail, outerResult+"|"+constructStringInner(board.head)+"|\n")
      } else outerResult + slice + "    B"
    }
    constructStringOuter(board, "    W\n" + slice)
  }
}

object Board {
  type boardT = Array[Array[Square]]
}
