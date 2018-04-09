package com.huginnmuninnresearch.chess.state

import scala.languageFeature.implicitConversions
import com.huginnmuninnresearch.chess.grading.Player
import com.huginnmuninnresearch.chess.pieces.Piece.{Index, Indices}
import com.huginnmuninnresearch.chess.pieces._
import Moves._

import scala.collection.mutable.ListBuffer

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

  def numberAttacking(owner: String, index : Index): Int = {
    val number = for {
      piece <- pieces(owner).filterNot(_.id == "King")
      ind <- piece.accessible(this)
      if index == ind
    } yield piece
    number.length + king(owner).totalCoverage(this).contains(index)
  }

  def moves(owner: String, mH: Gameplay): Gameplay = {
    (for {
      piece <- pieces(owner)
      index <- piece.legal(this, mH)
    } yield Move(piece, index, squares(idxToE(index)).piece, if (king(opponent(owner)).check(this)) CHECK else NORMAL)).to[ListBuffer]
  }

  def castle(owner: String, side: String, mH: Gameplay = ListBuffer()): Unit = {
    import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._ // for convenience
    import Piece._
    if (king(owner).castle(this, side)) {
      val row: String = if (owner == WHITE) "1" else "8"
      val king: (String, String) = if (side == KS) ("e", "g") else ("e", "b")
      val rook: (String, String) = if (side == KS) ("h", "f") else ("a", "c")
      squares(idxToE(aToI(king._2+row))).enter(squares(idxToE(aToI(king._1+row))).pop.get)
      squares(idxToE(aToI(rook._2+row))).enter(squares(idxToE(aToI(rook._1+row))).pop.get)
    }
  }

  def check(owner: String, mH: Gameplay): Boolean = king(owner).check(this)

  def promote(pawn: Pawn, choice: String): Unit = {
    require(Array("Bishop", "Knight", "Queen", "Rook").contains(choice))
    squares(idxToE(pawn.loc))
  }

  def king(owner: String): King = {
    val somePieces = pieces(owner)
    val king = somePieces.filter(_.id == "King")
    assert(king.nonEmpty, "Each side should have a King")
    king.head.asInstanceOf[King]
  }

  def result(justMoved: String, mH: Gameplay): Option[String] = {
    if (king(opponent(justMoved)).checkmate(this, mH)) Some(justMoved) else None
  }

  def opponent(owner: String): String = {
    owner match {
      case WHITE => BLACK
      case BLACK => WHITE
    }
  }

  def unmove(move: Move): Unit = {
    squares(idxToE(move.to)).fill(move.taken) // puts any taken piece back on the board
    squares(idxToE(move.piece.loc)).fill(move.piece) // puts the moved piece back where it came from
  }

  def unplay(moves: Gameplay): Unit = moves.reverse.foreach(unmove) // play through backwards

  def play(moves: Gameplay): Unit = moves.foreach(move)

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

  private def idxToE(index: Index): Int = SIDE*index._1 + index._2

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

  def verify(move: Move, mH: Gameplay): Boolean = {
    moves(move.piece.owner, mH).contains(move)
  }

  def create(piece: Piece, to: Index, mH: Gameplay): Board = {
    if (piece.legal(this, mH).contains(to)) squares(idxToE(to)).enter(squares(idxToE(piece.loc)).pop.get)
    this
  }

  def replicate(mH: Gameplay): Board = {
    val duplicate = new Board(white, black)
    duplicate.play(mH)
    duplicate
  }
  
  private def grabRow(row: Int): Squares = (for (col <- 0 until SIDE) yield squares(idxToE((row, col)))).to[Array]

  private def grabCol(col: Int): Squares = (for (row <- 0 until SIDE) yield squares(idxToE((row, col)))).to[Array]

  def forcePiecesByRow(owner: String, row: Int): Pieces = pieces(owner).filter(_.loc._1 == row)

  def forcePiecesByCol(owner: String, col: Int): Pieces = pieces(owner).filter(_.loc._2 == col)

  def piecesByRow(row: Int): Pieces = forcePiecesByRow(WHITE, row) ++ forcePiecesByRow(BLACK, row)

  def piecesByCol(col: Int): Pieces = forcePiecesByCol(WHITE, col) ++ forcePiecesByCol(BLACK, col)

  def propose(piece: Piece, to: Index, mH: Gameplay): Board = {
    val future = replicate(mH)
    future.move(piece.copy(), to)
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

  def move(m: Move): Unit = {
    import com.huginnmuninnresearch.chess.pieces.Piece._
    import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
    if (m.castling) {
      val side = if (m.to._2 == aToC("g")) KS else QS
      castle(m.piece.owner, side)
    } else {
      move(m.piece, m.to)
    }
  }

  val squares: Squares = initialiseBoard

  override def toString: String = {
    val slice: String = "----------\n"

    def stringifyBoard(squares: Squares): String = {
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

    "    B\n" + slice + stringifyBoard(squares.reverse) + slice + "    W\n"
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

  final val CHECK: String = "Check"
  final val NORMAL: String = ""
}
