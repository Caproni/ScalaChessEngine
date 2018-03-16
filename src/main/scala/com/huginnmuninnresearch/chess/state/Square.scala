package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.pieces.Piece
import com.huginnmuninnresearch.chess.pieces.Piece.{Index, Indices}

class Square(val loc: Index, private var _piece: Option[Piece]) {

  def this(row: Int, col: Int) = this((row, col), None)
  def this(loc: Index) = this(loc, None)

  def legal(board: Board, moveHistory: Indices): Indices = ???

  def colour: Chess.Value = {
    (loc._1 + loc._2) % 2 match {
      case 0 => Chess.White
      case _ => Chess.Black
    }
  }

  def pop: Option[Piece] = {
    val piece = piece
    _piece = None
    piece
  }

  def fill(piece: Piece): Unit = {
    _piece = Some(piece)
  }

  def capture(attacker: Piece): Option[Piece] = {
    val captured = piece
    _piece = Some(attacker)
    captured
  }

  def piece: Option[Piece] = {
    _piece
  }

  override def toString: String = {
    def squareCase: String = {
      colour match {
        case Chess.White => " "
        case _ => "."
      }
    }
    _piece.getOrElse(None) match {
      case None => squareCase
      case _ => _piece.get.toString
    }
  }
}

object Square {
  def apply(loc: Index, contents: Option[Piece]): Square = new Square(loc, contents)
}