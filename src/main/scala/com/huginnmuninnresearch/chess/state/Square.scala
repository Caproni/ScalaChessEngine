package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.notation.AlgebraicNotation.iToA
import com.huginnmuninnresearch.chess.pieces.Piece
import com.huginnmuninnresearch.chess.pieces.Piece.{Index, Indices}
import com.huginnmuninnresearch.chess.state.Board._
import com.huginnmuninnresearch.chess.state.Moves.Gameplay
import com.typesafe.scalalogging.LazyLogging

class Square(val loc: Index, private var _piece: Option[Piece]) extends LazyLogging {

  def this(row: Int, col: Int) = this((row, col), None)
  def this(loc: Index) = this(loc, None)

  val location: String = iToA(loc)

  def legal(b: Board, mH: Gameplay): Indices = {
    if (_piece == None) Array() else _piece.get.legal(b, mH)
  }

  private def colour: String = {
    (loc._1 + loc._2) % 2 match {
      case 0 => WHITE
      case _ => BLACK
    }
  }

  def pop: Option[Piece] = {
    val piece: Option[Piece] = _piece
    empty()
    piece
  }

  def empty(): Unit = {
    _piece = None
  }

  def enter(piece: Piece): Unit = {
    import Piece._
    val moved = true
    _piece = Some(pieceInstance(piece.id, piece.owner, loc, moved))
  }

  def fill(piece: Piece): Unit = {
    _piece = Some(piece)
  }

  def fill(piece: Option[Piece]): Unit = {
    _piece = piece
  }

  def capture(attacker: Piece): Option[Piece] = {
    import Piece._
    val captured = piece
    val moved = true
    _piece = Some(pieceInstance(attacker.id, attacker.owner, loc, moved))
    captured
  }

  def piece: Option[Piece] = {
    _piece
  }

  override def toString: String = {
    def squareCase: String = {
      colour match {
        case WHITE => "."
        case BLACK => " "
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