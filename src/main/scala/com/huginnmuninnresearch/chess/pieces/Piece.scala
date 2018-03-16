package com.huginnmuninnresearch.chess.pieces

import Piece._
import com.huginnmuninnresearch.chess.state.Board._
import com.huginnmuninnresearch.chess.state.{Board, Chess, Move}


abstract sealed class Piece(val owner: Chess.Value, var loc: index) {
  val id: String
  def coverage: indices // accessible squares not accounting for other pieces on the board
  def attack(ps: pieces): indices // accessible squares accounting for other pieces on the board
  def legal(b: Board, moveHistory: Array[Move]): indices
  def toString: String
  protected def pieceCase(name: String): String = {
    owner match {
      case Chess.White => name.toString.toUpperCase
      case _ => name.toString.toLowerCase
    }
  }
}

case class Bishop(override val owner: Chess.Value, override var loc: index) extends Piece(owner, loc) {
  override val id: String = "Bishop"
  override def coverage: indices = ???
  override def attack(ps: pieces): indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): indices = ???
  override def toString: String = pieceCase("B")
}

case class King(override val owner: Chess.Value, override var loc: index) extends Piece(owner, loc) {
  override val id: String = "King"
  override def coverage: indices = {
    for (col <- downSelect(loc._1); row <- downSelect(loc._2)) yield (row, col)
  }
  override def attack(ps: pieces): indices = {
    for {col <- downSelect(loc._1)
         row <- downSelect(loc._2)
         if ps.getorElse(true)} yield (row, col)
  }

  override def legal(b: Board, moveHistory: Array[Move]): indices = ???

  private def downSelect(index: Int): Array[Int] = {
    index match {
      case 0 => Array(index, index+1)
      case 7 => Array(index-1, index)
      case _ => Array(index-1, index, index+1)
    }
  }
  override def toString: String = pieceCase("K")
}

case class Knight(override val owner: Chess.Value, override var loc: index) extends Piece(owner, loc) {
  override val id: String = "Knight"
  override def coverage: indices = ???
  override def attack(ps: pieces): indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): indices = ???
  override def toString: String = pieceCase("N")
}

case class Pawn(override val owner: Chess.Value, override var loc: index) extends Piece(owner, loc) {
  override val id: String = "Pawn"
  override def coverage: indices = ???
  override def attack(ps: pieces): indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): indices = ???
  override def toString: String = pieceCase("P")
}

case class Queen(override val owner: Chess.Value, override var loc: index) extends Piece(owner, loc) {
  override val id: String = "Queen"
  override def coverage: indices = ???
  override def attack(ps: pieces): indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): indices = ???
  override def toString: String = pieceCase("Q")
}

case class Rook(override val owner: Chess.Value, override var loc: index) extends Piece(owner, loc) {
  override val id: String = "Rook"
  override def coverage: indices = ???
  override def attack(ps: pieces): indices = ???
  override def legal(b: Board, moveHistory: Array[Move]): indices = ???
  override def toString: String = pieceCase("R")
}

object Piece {
  type index = (Int, Int)
  type indices = Array[index]
}
