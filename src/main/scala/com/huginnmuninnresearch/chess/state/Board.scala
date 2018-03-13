package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.pieces.{Pawn, Knight, Bishop, Rook, Queen, King}

import scala.annotation.tailrec

class Board(white: Player, black: Player, var squares: Array[Square]) {
  import Board._
  def this(white: Player, black: Player) = this(white, black, {
    val empty = Array.ofDim[Square](Chess.boardSize)
    for (e <- 0 until Chess.boardSize) {
      empty(e) = Square(e % Chess.sideSize, e / Chess.sideSize, None)
    }
    empty
  })

  private def populateMajorPieces(b: boardT, row: Int, owner: Chess.Value): boardT = {
    for (col <- 0 until Chess.sideSize) {
      col match {
        case 0 | 7 => b(row)(col) = Square(row, col, Some(Rook(owner, (row, col))))
        case 1 | 6 => b(row)(col) = Square(row, col, Some(Knight(owner, (row, col))))
        case 2 | 5 => b(row)(col) = Square(row, col, Some(Bishop(owner, (row, col))))
        case 4 => b(row)(col) = Square(row, col, Some(Queen(owner, (row, col))))
        case 3 => b(row)(col) = Square(row, col, Some(King(owner, (row, col))))
      }
    }
    b
  }

  private def initialiseBoard: boardT = {
    for (e <- 0 until Chess.boardSize) {
      e * Chess.sideSize match {
        case 0 => populateMajorPieces(squares, e, Chess.White)
        case 7 => populateMajorPieces(squares, e, Chess.Black)
        case 1 => for (col <- 0 until Chess.sideSize) {squares(e)(col) = Square(row, col, Some(Pawn(Chess.White, (row, col))))}
        case 6 => for (col <- 0 until Chess.sideSize) {squares(e)(col) = Square(row, col, Some(Pawn(Chess.Black, (row, col))))}
        case _ => ()
      }
    }
    squares
  }

  def movePiece(from: (Int, Int), to: (Int, Int)): Unit = {
    squares(from._1)(from._2) = new Square(from._1, from._2)

  }

  squares = initialiseBoard

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
    constructStringOuter(squares, "    W\n" + slice)
  }
}

object Board {
  type boardT = Array[Square]
}
