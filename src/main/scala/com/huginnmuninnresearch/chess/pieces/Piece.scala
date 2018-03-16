package com.huginnmuninnresearch.chess.pieces

import Piece._
import com.huginnmuninnresearch.chess.state.Board._
import com.huginnmuninnresearch.chess.state.{Board, Chess, Move}

import scala.annotation.tailrec


abstract sealed class Piece(val owner: Chess.Value, var loc: Index) {
  val id: String
  def totalCoverage: Indices // accessible squares not accounting for other pieces on the board
  def accessible(ps: Pieces): Indices // accessible squares accounting for other pieces on the board
  def attack(b: Board, ps: Pieces, mH: Array[Move]): Pieces // enemy pieces which are attacked by this piece
  def defend(ps: Pieces): Pieces // friendly pieces which are defended by this piece
  def legal(b: Board, moveHistory: Array[Move]): Indices // legal moves for this piece
  def toString: String
  protected def pieceCase(name: String): String = {
    owner match {
      case Chess.White => name.toString.toUpperCase
      case _ => name.toString.toLowerCase
    }
  }
  protected def onBoard(index: Index): Boolean = {
    if (0 <= index._1 && 0 <= index._2 && index._1 < sideSize && index._2 < sideSize) true else false
  }
}

case class Bishop(override val owner: Chess.Value, override var loc: Index) extends Piece(owner, loc) {
  override val id: String = "Bishop"
  override def totalCoverage: Indices = {
    def downSelect(index: Index): Indices = {
      val coverage: Indices = Array()
      for (x <- -sideSize until sideSize) {
        val locationA = (loc._1+x, loc._2+x) // diagonal
        if (onBoard(locationA)) coverage:+locationA
        val locationB = (loc._1+x, loc._2-x) // perpendicular
        if (onBoard(locationB)) coverage:+locationB
      }
      coverage
    }
    downSelect(loc)
  }
  override def attack(b: Board, ps: Pieces, mH: Array[Move]): Pieces = {
    for {p <- ps; if legal(b, mH).foldLeft(false)(_ || _ == p.loc)} yield p
  }
  override def defend(ps: Pieces): Pieces = ???

  override def legal(b: Board, mH: Array[Move]): Indices = {
//    if (moveHistory.head.result == Move.check) true else false
    ???
  }

  override def toString: String = pieceCase("B")
}

case class King(override val owner: Chess.Value, override var loc: Index) extends Piece(owner, loc) {
  override val id: String = "King"
  override def totalCoverage: Indices = {
    for (col <- downSelect(loc._1); row <- downSelect(loc._2)) yield (row, col)
  }
  override def attack(ps: Pieces): Indices = {
    for {col <- downSelect(loc._1)
         row <- downSelect(loc._2)
         if ps.getorElse(true)} yield (row, col)
  }

  override def legal(b: Board, moveHistory: Array[Move]): Indices = ???

  private def downSelect(index: Int): Array[Int] = {
    index match {
      case 0 => Array(index, index+1)
      case 7 => Array(index-1, index)
      case _ => Array(index-1, index, index+1)
    }
  }
  override def toString: String = pieceCase("K")
}

case class Knight(override val owner: Chess.Value, override var loc: Index) extends Piece(owner, loc) {
  override val id: String = "Knight"
  override def totalCoverage: Indices = ???
  override def attack(ps: Pieces): Indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): Indices = ???
  override def toString: String = pieceCase("N")
}

case class Pawn(override val owner: Chess.Value, override var loc: Index) extends Piece(owner, loc) {
  override val id: String = "Pawn"
  override def totalCoverage: Indices = ???
  override def attack(ps: Pieces): Indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): Indices = ???
  override def toString: String = pieceCase("P")
}

case class Queen(override val owner: Chess.Value, override var loc: Index) extends Piece(owner, loc) {
  override val id: String = "Queen"
  override def totalCoverage: Indices = ???
  override def attack(ps: Pieces): Indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): Indices = ???
  override def toString: String = pieceCase("Q")
}

case class Rook(override val owner: Chess.Value, override var loc: Index) extends Piece(owner, loc) {
  override val id: String = "Rook"
  override def totalCoverage: Indices = ???
  override def attack(ps: Pieces): Indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): Indices = ???
  override def toString: String = pieceCase("R")
}

object Piece {
  type Index = (Int, Int)
  type Indices = Array[Index]
}
