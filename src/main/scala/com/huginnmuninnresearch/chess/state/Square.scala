package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.pieces.Piece

class Square(val row: Int, val col: Int, private var contents: Option[Piece]) {
  def this(row: Int, col: Int) = this(row, col, Chess.Empty, None)

  def colour: Chess.Value = {
    (row + col) % 2 match {
      case 0 => Chess.White
      case _ => Chess.Black
    }
  }

  def empty(): Unit = {
    contents = None
  }

  def fill(piece: Piece): Unit = {
    contents = Some(piece)
  }

  override def toString: String = {
    def pieceCase(piece: Piece): String = {
      piece.owner match {
        case Chess.White => piece.toString.toUpperCase
        case _ => piece.toString.toLowerCase
      }
    }
    def squareCase(row: Int, col: Int): String = {
      colour match {
        case Chess.White => " "
        case _ => "."
      }
    }
    contents.getOrElse(None) match {
      case None => squareCase(row, col)
      case _ => pieceCase(contents.get)
    }
  }
}

object Square {
  def apply(row: Int, col: Int, contents: Option[Piece]): Square = new Square(row, col, contents)
}