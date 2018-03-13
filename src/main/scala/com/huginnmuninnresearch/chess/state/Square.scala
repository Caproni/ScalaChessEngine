package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.pieces.Piece

class Square(val row: Int, val col: Int, private var contents: Option[(Piece, ChessParameters.Value)]) {
  def this(row: Int, col: Int) = this(row, col, ChessParameters.Empty, None)

  def colour: ChessParameters.Value = {
    (row + col) % 2 match {
      case 0 => ChessParameters.White
      case _ => ChessParameters.Black
    }
  }

  def empty(): Unit = {
    contents = None
  }

  def fill(piece: Piece, owner: ChessParameters.Value): Unit = {
    contents = Some(piece, owner)
  }

  override def toString: String = {
    def pieceCase(side: Option[ChessParameters.Value], x: String): String = {
      side.get match {
        case ChessParameters.White => x
        case _ => x.toLowerCase
      }
    }
    def squareCase(row: Int, col: Int): String = {
      colour match {
        case ChessParameters.White => " "
        case _ => "."
      }
    }
    contents.getOrElse(None) match {
      case None => squareCase(row, col)
      case contents.get._1 => pieceCase(contents.get._2, ChessParameters.PieceNotation.Pawn.toString)
      case ChessParameters.King => pieceCase(side, ChessParameters.PieceNotation.King.toString)
      case ChessParameters.Queen => pieceCase(side, ChessParameters.PieceNotation.Queen.toString)
      case ChessParameters.Rook => pieceCase(side, ChessParameters.PieceNotation.Rook.toString)
      case ChessParameters.Bishop => pieceCase(side, ChessParameters.PieceNotation.Bishop.toString)
      case ChessParameters.Knight => pieceCase(side, ChessParameters.PieceNotation.Knight.toString)
    }
  }

}

object Square {
  def apply(row: Int, col: Int, contents: ChessParameters.Value, side: Option[ChessParameters.Value]): Square = new Square(row, col, contents, side)
}