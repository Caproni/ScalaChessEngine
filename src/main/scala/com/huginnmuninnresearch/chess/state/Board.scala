package com.huginnmuninnresearch.chess.state

import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.notation.AlgebraicNotation.aToI
import com.huginnmuninnresearch.chess.pieces.Piece.{Index, Indices}
import com.huginnmuninnresearch.chess.pieces._
import com.huginnmuninnresearch.chess.record.MoveHistory
import com.huginnmuninnresearch.chess.state.Moves._

import scala.collection.mutable.ListBuffer
import scala.languageFeature.implicitConversions

class Board(val white: Player, val black: Player) {
  import Board._

  implicit def iversonBracket(b: Boolean): Int = if (b) 1 else 0

  private def populateMajorPieces(owner: String, squares: Squares, e: Int): Squares = {
    squares(e).loc match {
      case (_,0) | (_,7) => squares(e).fill(Rook(owner, squares(e).loc))
      case (_,1) | (_,6) => squares(e).fill(Knight(owner, squares(e).loc))
      case (_,2) | (_,5) => squares(e).fill(Bishop(owner, squares(e).loc))
      case (_,3) => squares(e).fill(Queen(owner, squares(e).loc))
      case (_,4) => squares(e).fill(King(owner, squares(e).loc))
      case (_,_) => ()
    }
    squares
  }

  private def totalAttacking(owner: String, index: Index, mH: MoveHistory) = {
    for {
      piece <- pieces(owner).filterNot(_.id == "King")
      if piece.accessible(this, mH).contains(index)
    } yield piece
  }

  def numberAttacking(owner: String, index : Index, mH: MoveHistory): Int = {
    val number = totalAttacking(owner, index, mH)
    number.length + king(owner).immediateCoverage(this).contains(index)
  }

  def piecesAttacking(owner: String, index : Index, mH: MoveHistory): Pieces = {
    val attackers = totalAttacking(owner, index, mH)
    if (king(owner).immediateCoverage(this).contains(index)) attackers :+ king(owner) else attackers
  }

  def moves(owner: String, mH: MoveHistory): Gameplay = {
    ListBuffer((for {
      piece <- pieces(owner)
      move <- piece.legal(this, mH)
    } yield move): _ *)
  }

  def stalemate(owner: String, mH: MoveHistory): Boolean = {
    if (moves(owner, mH).isEmpty) true else false
  }

  private def doCastle(row: String, piece: (String, String)): Unit = {
    squares(idxToE(aToI(piece._2 + row))).enter(squares(idxToE(aToI(piece._1 + row))).pop.get)
  }

  def castle(owner: String, side: String, mH: MoveHistory): Unit = {
    import Piece._
    if (king(owner).castle(this, side, mH)) {
      val row: String = if (owner == WHITE) "1" else "8"
      val king: (String, String) = if (side == KS) ("e", "g") else ("e", "b")
      val rook: (String, String) = if (side == KS) ("h", "f") else ("a", "c")
      doCastle(row, king)
      doCastle(row, rook)
    }
  }

  def check(owner: String, mH: MoveHistory): Boolean = king(owner).check(this, mH)

  def promote(pawn: Pawn, choice: String): Unit = {
    require(Array("B", "N", "Q", "R").contains(choice.toUpperCase))
    val newPiece = choice match {
      case "B" => Bishop(pawn.owner, pawn.loc, pawn.moved)
      case "N" => Knight(pawn.owner, pawn.loc, pawn.moved)
      case "Q" => Queen(pawn.owner, pawn.loc, pawn.moved)
      case "R" => Rook(pawn.owner, pawn.loc, pawn.moved)
    }
    squares(idxToE(pawn.loc)).fill(newPiece.asInstanceOf[Piece])
  }

  def canPromote(piece: Piece): Boolean = if (piece.id == "P" && ((piece.loc._1 == 0 && piece.owner == "Black") || (piece.loc._1 == 7 && piece.owner == "White"))) {
    true
  } else false

  def king(owner: String): King = {
    val somePieces = pieces(owner)
    val king = somePieces.filter(_.id == "King")
    assert(king.nonEmpty, "Each side should have a King")
    king.head.asInstanceOf[King]
  }

  def result(justMoved: String, mH: MoveHistory): Option[String] = {
    if (checkmate(justMoved, mH)) Some(justMoved) else None
  }

  def checkmate(owner: String, mH: MoveHistory): Boolean = king(opponent(owner)).checkmate(this, mH)

  def opponent(owner: String): String = {
    owner match {
      case WHITE => BLACK
      case BLACK => WHITE
    }
  }

  def undoMove(move: Move): Unit = {
    squares(idxToE(move.to)).fill(move.taken) // puts any taken piece back on the board
    squares(idxToE(move.piece.loc)).fill(move.piece) // puts the moved piece back where it came from
  }

  def undoPlay(mH: MoveHistory): Unit = mH.m.moves.reverse.foreach(undoMove) // play through backwards

  def play(mH: MoveHistory): Unit = {
    for (move <- mH.m.moves) {
      this.move(move.piece, move.to)
    }
  }

  def pieces: Pieces = for (square <- squares.filter(_.piece.nonEmpty)) yield square.piece.get

  def pieces(piece: Piece): Pieces = pieces.filter(_.id == piece.id)

  def pieces(owner: String): Pieces = pieces.filter(_.owner == owner)

  def pieces(owner: String, piece: Piece): Pieces = pieces.filter(_.id == piece.id).filter(_.owner == owner)

  def piece(e: Int): Option[Piece] = squares(e).piece

  def piece(index: Index): Option[Piece] = squares(idxToE(index)).piece

  def pieces(indices: Indices): Pieces = for {piece <- pieces; if indices.contains(piece.loc)} yield piece

  def pieceExists(index: Index): Boolean = piece(index).nonEmpty

  def forcePieceExists(index: Index, owner: String): Boolean = piece(index).nonEmpty && piece(index).get.owner == owner

  private def clean: Squares = {
    val empty = Array.ofDim[Square](BOARD)
    for (e <- 0 until BOARD) empty(e) = Square(EToIdx(e), None)
    empty
  }

  private def idxToE(index: Index): Int = SIDE * index._1 + index._2

  private def EToIdx(e: Int): Index = (e / SIDE, e % SIDE)

  private def initialiseBoard: Squares = {
    val empty: Squares = clean
    for (e <- 0 until BOARD) {
      empty(e).loc match {
        case (0,_) => populateMajorPieces(WHITE, empty, e)
        case (7,_) => populateMajorPieces(BLACK, empty, e)
        case (1,_) => empty(e).fill(Pawn(WHITE, empty(e).loc))
        case (6,_) => empty(e).fill(Pawn(BLACK, empty(e).loc))
        case (_,_) => ()
      }
    }
    empty
  }

  def verify(move: Move, mH: MoveHistory): Boolean = {
    moves(move.piece.owner, mH).contains(move)
  }

  def create(piece: Piece, to: Index, mH: MoveHistory): Board = {
    if (piece.legal(this, mH).contains(to)) squares(idxToE(to)).enter(squares(idxToE(piece.loc)).pop.get)
    this
  }

  def replicate(mH: MoveHistory): Board = {
    val duplicate = new Board(white, black)
    duplicate.play(mH)
    duplicate
  }
  
  private def grabRow(row: Int): Squares = (for (col <- 0 until SIDE) yield squares(idxToE((row, col)))).toArray

  private def grabCol(col: Int): Squares = (for (row <- 0 until SIDE) yield squares(idxToE((row, col)))).toArray

  def forcePiecesByRow(owner: String, row: Int): Pieces = pieces(owner).filter(_.loc._1 == row)

  def forcePiecesByCol(owner: String, col: Int): Pieces = pieces(owner).filter(_.loc._2 == col)

  def piecesByRow(row: Int): Pieces = forcePiecesByRow(WHITE, row) ++ forcePiecesByRow(BLACK, row)

  def piecesByCol(col: Int): Pieces = forcePiecesByCol(WHITE, col) ++ forcePiecesByCol(BLACK, col)

  def propose(piece: Piece, to: Index, mH: MoveHistory): Board = {
    val future = replicate(mH)
    future.move(piece.copy, to)
    future
  }

  private def move(piece: Piece, to: Index): Unit = {
    require(!Move(piece, to, None).castling, "Not for castling. Use move(m: Move) instead")
    squares(idxToE(piece.loc)).empty()
    squares(idxToE(to)).enter(piece)
  }

  def move(piece: Piece, to: String): Unit = {
    import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
    move(piece, aToI(to))
  }

  def move(m: Move, mH: MoveHistory): Unit = {
    import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
    import com.huginnmuninnresearch.chess.pieces.Piece._
    if (m.castling) {
      val side = if (m.to._2 == aToC("g")) KS else QS
      castle(m.piece.owner, side, mH)
    } else {
      squares(idxToE(m.piece.loc)).empty()
      val movedPiece = Pawn(m.piece.owner, m.to, m.piece.moved)
      if (canPromote(m.piece)) {
        promote(movedPiece, m.result.substring(0, 1))
      } else squares(idxToE(m.to)).enter(m.piece)
    }
  }

  val squares: Squares = initialiseBoard

  override def toString: String = {
    val slice: String = "----------\n"

    def stringifyBoard(): String = {
      var result = ""
      for (row <- 0 until SIDE) {
        result = stringifyRow(grabRow(row))++result
      }
      result
    }

    def stringifyRow(squares: Squares): String = {
      val row = for (square <- squares) yield wrap(square)
      row.foldRight("")(_ + _)
    }

    def wrap(square: Square): String = {
      square.location.substring(0, 1) match {
        case "a" => "|"+square.toString
        case "h" => square.toString+"|\n"
        case _ => square.toString
      }
    }

    "    B\n" + slice + stringifyBoard() + slice + "    W\n"
  }
}

object Board {
  type Squares = Array[Square]
  type Pieces = Array[Piece]

  final val SIDE: Int = 8 // length of a row or column
  final val BOARD: Int = 64 // number of squares on a chess board

  final val WHITE: String = "White"
  final val BLACK: String = "Black"
  final val DRAW: String = "Draw"

}
