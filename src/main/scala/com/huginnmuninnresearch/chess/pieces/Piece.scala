package com.huginnmuninnresearch.chess.pieces

import com.huginnmuninnresearch.chess.state.{Board, Chess, Square}

abstract sealed class Piece(val owner: Chess.Value, var location: (Int, Int)) {
  val id: String
  def coverage: Array[(Int, Int)] // accessible squares not accounting for other pieces on the board
  def attack(board: Board): Array[(Int, Int)] // accessible squares accounting for other pieces on the board
//  def legal(board: Board, moveHistory: Array[(Int, Int)]): Array[(Int, Int)] // TODO
  def toString: String
}

case class Bishop(override val owner: Chess.Value, override var location: (Int, Int)) extends Piece(owner, location) {
  override val id: String = "Bishop"
  override def coverage: Array[Square] = ???
  override def attack(board: Board): Array[Square] = ???
  override def toString: String = "B"
}

case class King(override val owner: Chess.Value, override var location: (Int, Int)) extends Piece(owner, location) {
  override val id: String = "King"
  override def coverage: Array[(Int, Int)] = {
    for (col <- downSelect(location._1); row <- downSelect(location._2)) yield (row, col)
  }
  override def attack(b: Board): Array[(Int, Int)] = {
    for {col <- downSelect(location._1)
         row <- downSelect(location._2)
         if b.squares(row)(col)..getorElse(true)} yield (row, col)
  }

  private def downSelect(index: Int): Array[Int] = {
    index match {
      case 0 => Array(index, index+1)
      case 7 => Array(index-1, index)
      case _ => Array(index-1, index, index+1)
    }
  }
  override def toString: String = "K"
}

case class Knight(override val owner: Chess.Value, override var location: (Int, Int)) extends Piece(owner, location) {
  override val id: String = "Knight"
  override def coverage: Array[(Int, Int)] = ???
  override def attack(b: Board): Array[(Int, Int)] = coverage
  override def toString: String = "N"
}

case class Pawn(override val owner: Chess.Value, override var location: (Int, Int)) extends Piece(owner, location) {
  override val id: String = "Pawn"
  override def coverage: Array[(Int, Int)] = ???
  override def attack(b: Board): Array[(Int, Int)] = ???
  override def toString: String = "P"
}

case class Queen(override val owner: Chess.Value, override var location: (Int, Int)) extends Piece(owner, location) {
  override val id: String = "Queen"
  override def coverage: Array[(Int, Int)] = ???
  override def attack(b: Board): Array[(Int, Int)] = ???
  override def toString: String = "Q"
}

case class Rook(override val owner: Chess.Value, override var location: (Int, Int)) extends Piece(owner, location) {
  override val id: String = "Rook"
  override def coverage: Array[(Int, Int)] = ???
  override def attack(b: Board): Array[(Int, Int)] = ???
  override def toString: String = "R"
}
