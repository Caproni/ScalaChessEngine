package com.huginnmuninnresearch.chess.notation

import com.huginnmuninnresearch.chess.pieces.Piece.Index
import com.huginnmuninnresearch.chess.state.Moves.Gameplay
import com.huginnmuninnresearch.chess.state.{Board, Move}
import com.typesafe.scalalogging.LazyLogging

object AlgebraicNotation extends LazyLogging {

  def parse(implicit b: Board, mover: String, s: String, mH: Gameplay): Option[Move] = {
    ???
//    import com.huginnmuninnresearch.chess.pieces.Piece._
//    import Board._
//    val len = s.length
//    if (MAJORPIECES.contains(determinePiece(s.substring(0, 1)).toUpperCase)) { // piece move
//      ???
//    } else if (s.substring(1, 2) == "-") { // castles
//      val row = if (mover == WHITE) "1" else "8"
//      val castle: Boolean = true
//      if (len == 3) { // castle King's side
//        if (b.king(mover).castle(b, KS, mH)) Some(Move(b.king(mover), aToI("G" + row), None, castle)) else None
//      } else if (len == 5) { // castle Queen's side
//        if (b.king(mover).castle(b, QS, mH)) Some(Move(b.king(mover), aToI("C" + row), None, castle)) else None
//      } else { // no match to castling
//        None
//      }
//    } else { // pawn move
//      if (len == 2 && ROWS.contains(s.substring(1, 2)) { // simple pawn push
//        b.forcePiecesByCol(aToI(s))
//        Some(Move())
//      } else if (len == 2 && COLUMNS.contains(s.substring(1, 2)) { // simple pawn capture
//        Some(Move(b.piece(), aToI(s)))
//      } else None // no match
//    }
  }

  def aToR(row: String): Int = {
    require(ROWS.contains(row))
    row.toInt-1
  }

  def aToC(col: String): Int = {
    require(COLUMNS.contains(col.substring(0, 1).toLowerCase))
    col match {
      case "a" => 0
      case "b" => 1
      case "c" => 2
      case "d" => 3
      case "e" => 4
      case "f" => 5
      case "g" => 6
      case "h" => 7
    }
  }

  def rToA(row: Int): String = {
    require(INDEX.contains(row))
    (row.toInt+1).toString
  }

  def cToA(col: Int): String = {
    require(INDEX.contains(col))
    col match {
      case 0 => "a"
      case 1 => "b"
      case 2 => "c"
      case 3 => "d"
      case 4 => "e"
      case 5 => "f"
      case 6 => "g"
      case 7 => "h"
    }
  }

  def aToI(square: String): Index = {
    require(square.length == 2)
    (aToR(square.substring(1, 2)), aToC(square.substring(0, 1).toLowerCase))
  }

  def iToA(square: Index): String = {
    cToA(square._2) + rToA(square._1)
  }

  final val CASTLEKINGS: String = "O-O"
  final val CASTLEQUEENS: String = "O-O-O"
  final val CHECK = "+"
  final val DOUBLECHECK = "++"
  final val CHECKMATE = "#"

  private def determinePiece(s: String): String = {
    s.substring(0, 1) match {
      case "P" => "Pawn"
      case "B" => "Bishop"
      case "N" => "Knight"
      case "R" => "Rook"
      case "Q" => "Queen"
      case "K" => "King"
    }
  }

  final val ROWS = Array("1", "2", "3", "4", "5", "6", "7", "8")
  final val INDEX = Array(0, 1, 2, 3, 4, 5, 6, 7)
  final val MAJORPIECES = Array("B", "N", "R", "Q", "K")
  final val COLUMNS = Array("a", "b", "c", "d", "e", "f", "g", "h")
  final val PAWN = "P"

}
