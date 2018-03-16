package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.pieces._

import scala.annotation.tailrec

class Board(val white: Player, val black: Player) {
  import Board._

  private def populateMajorPieces(owner: Chess.Value, squares: Squares, e: Int): Squares = {
    squares(e).loc match {
      case (_,0) | (_,7) => squares(e).fill(Rook(owner, squares(e).loc))
      case (_,1) | (_,6) => squares(e).fill(Knight(owner, squares(e).loc))
      case (_,2) | (_,5) => squares(e).fill(Bishop(owner, squares(e).loc))
      case (_,4) => squares(e).fill(Queen(owner, squares(e).loc))
      case (_,3) => squares(e).fill(King(owner, squares(e).loc))
    }
    squares
  }

  def pieces: Pieces = {
    val pieces = for {
      e <- 0 until boardSize
      if squares(e).piece.nonEmpty
    } yield squares(e).piece.get
    pieces.toArray
  }

  def pieces(owner: Chess.Value): Pieces = {
    pieces.filter(_.owner == owner)
  }

  private def clean: Squares = {
    val empty = Array.ofDim[Square](boardSize)
    for (e <- 0 until boardSize) {
      empty(e) = Square((e % sideSize, e / sideSize), None)
    }
    empty
  }

  private def initialiseBoard: Squares = {
    val empty: Squares = clean
    for (e <- 0 until boardSize) {
      empty(e).loc match {
        case (0,_) => populateMajorPieces(Chess.White, empty, e)
        case (7,_) => populateMajorPieces(Chess.Black, empty, e)
        case (1,_) => empty(e).fill(Pawn(Chess.White, empty(e).loc))
        case (6,_) => empty(e).fill(Pawn(Chess.Black, empty(e).loc))
        case (_,_) => ()
      }
    }
    empty
  }

  def movePiece(From: (Int, Int), To: (Int, Int)): Unit = {
    for (e <- 0 until boardSize) yield squares(e).loc match {
      case From => {require(squares(e).piece.getOrElse(None) != None); squares(e).piece.legal(squares, To)}
      case To => squares(e).piece.
      case _ => ()
    }
  }

  val squares: Squares = initialiseBoard

  override def toString: String = {
    val slice: String = "----------\n"
    @tailrec
    def constructString(squares: Squares, result: String): String = {
      if (squares.nonEmpty) {
        def wrap(square: Square): String = {
          square.loc._2 match {
            case 0 => "|"+square.toString
            case sideSize.-(1) => square.toString+"|\n"
            case _ => square.toString
          }
        }
        constructString(squares.tail, result++wrap(squares.head)+"|\n")
      } else result + slice + "    B"
    }
    constructString(squares, "    W\n" + slice)
  }
}

object Board {
  type Squares = Array[Square]
  type Pieces = Array[Piece]

  final val sideSize: Int = 8 // length of a row or column
  final val boardSize: Int = 64 // number of squares on a chess board
}
