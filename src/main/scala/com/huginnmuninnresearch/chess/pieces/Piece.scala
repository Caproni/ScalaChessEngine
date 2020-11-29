package com.huginnmuninnresearch.chess.pieces

import com.huginnmuninnresearch.chess.state.Board._
import com.huginnmuninnresearch.chess.state.{Board, Move}
import com.huginnmuninnresearch.chess.state.Moves.Gameplay
import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
import Piece._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ListBuffer

abstract sealed class Piece(val owner: String, val loc: Index, val moved: Boolean) extends LazyLogging {

  val id: String
  val location: String

  def totalCoverage(implicit b: Board, mH: Gameplay): Indices // accessible squares not accounting for other pieces on the board
  def accessible(implicit b: Board, moveHistory: Gameplay = ListBuffer()): Indices // accessible squares accounting for other pieces on the board
  def toString: String

  def copy: Piece

  protected def printLegalMoves(tO: TraversableOnce[Index]): Unit = {
    println(s"$owner $id on ${location.toLowerCase} has legal moves:")
    if (tO.nonEmpty) tO.map(iToA(_).toLowerCase + " ").foreach(print)
    println
  }

  protected def bishopAccess(implicit b: Board): Indices = {

    val nE: Indices = (for { // TODO inefficient code
      d <- 1 until SIDE
      blocker = for (
        dSub <- 1 until d
        if onBoard((loc._1+d, loc._2+d))
      ) yield b.pieceExists((loc._1+dSub, loc._2+dSub))
      if onBoard((loc._1+d, loc._2+d)) && !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((loc._1+d, loc._2+d), owner))
    } yield (loc._1+d, loc._2+d)).to[Array]

    val sW: Indices = (for {
      d <- 1 until SIDE
      blocker = for (
        dSub <- 1 until d
        if onBoard((loc._1-d, loc._2-d))
      ) yield b.pieceExists((loc._1-dSub, loc._2-dSub))
      if onBoard((loc._1-d, loc._2-d)) && !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((loc._1-d, loc._2-d), owner))
    } yield (loc._1-d, loc._2-d)).to[Array]

    val nW: Indices = (for {
      d <- 1 until SIDE
        blocker = for (
          dSub <- 1 until d
          if onBoard((loc._1+d, loc._2-d))
        ) yield b.pieceExists((loc._1 + dSub, loc._2 - dSub))
        if onBoard((loc._1+d, loc._2-d)) && !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((loc._1 + d, loc._2 - d), owner))
    } yield (loc._1+d, loc._2-d)).to[Array]

    val sE: Indices = (for {
      d <- 1 until SIDE
      blocker = for (
        dSub <- 1 until d
        if onBoard((loc._1-d, loc._2+d))
      ) yield b.pieceExists((loc._1-dSub, loc._2+dSub))
      if onBoard((loc._1-d, loc._2+d)) && !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((loc._1-d, loc._2+d), owner))
    } yield (loc._1-d, loc._2+d)).to[Array]

    val accessible: Indices = (sW ++: sE ++: nW ++: nE).distinct.filterNot(_ == loc)
    accessible
  }

  protected def rookAccess(implicit b: Board): Indices = {
    val east: Indices = (for {
      x <- loc._2 + 1 until SIDE
      blocker = for (sub <- loc._2 + 1 until x) yield b.pieceExists((loc._1, sub))
      if !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((loc._1, x), owner))
    } yield (loc._1, x)).to[Array]

    val west: Indices = (for {
      x <- 0 until loc._2
      blocker = for (sub <- x + 1 until loc._2) yield b.pieceExists((loc._1, sub))
      if !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((loc._1, x), owner))
    } yield (loc._1, x)).to[Array]

    val north: Indices = (for {
      x <- loc._1 + 1 until SIDE
      blocker = for (sub <- loc._1 + 1 until x) yield b.pieceExists((sub, loc._2))
      if !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((x, loc._2), owner))
    } yield (x, loc._2)).to[Array]

    val south: Indices = (for {
      x <- 0 until loc._1
      blocker = for (sub <- x + 1 until loc._1) yield b.pieceExists((sub, loc._2))
      if !(blocker.foldLeft(false)(_ || _) || b.forcePieceExists((x, loc._2), owner))
    } yield (x, loc._2)).to[Array]

    val accessible: Indices = north ++: south ++: east ++: west
    accessible
  }

  def attack(implicit b: Board, mH: Gameplay): Pieces = {
    val indexes = {
      for (index <- accessible; if b.forcePieceExists(index, b.opponent(owner))) yield index
    }
    b.pieces(indexes)
  }

  def defend(implicit b: Board, mH: Gameplay): Pieces = {
    val indexes = {
      for (index <- accessible; if b.forcePieceExists(index, owner)) yield index
    }
    b.pieces(indexes)
  }

  def legal(implicit b: Board, moveHistory: Gameplay): Indices = { // legal moves for this piece
    import com.huginnmuninnresearch.chess.notation.AlgebraicNotation._
    val indices = accessible(b, moveHistory).filterNot(_ == b.king(b.opponent(owner)).loc).filterNot(_ == b.king(owner).loc)
    var legalMoves: Indices = Array[Index]()
    for (index <- indices) {
      val future = b.propose(this, index, moveHistory)
      val fMH = moveHistory :+ Move(this, index, b.piece(index), if (future.check(future.opponent(owner), moveHistory)) CHECK else NORMAL)
      legalMoves = if (!future.check(owner, fMH)) legalMoves :+ index else legalMoves
    }
//    printLegalMoves(legalMoves)
    legalMoves
  }

  protected def pieceCase(name: String): String = {
    owner match {
      case WHITE => name.toString.toUpperCase
      case BLACK => name.toString.toLowerCase
    }
  }

  protected def onBoard(index: Index): Boolean = {
    if (0 <= index._1 && 0 <= index._2 && index._1 < SIDE && index._2 < SIDE) true else false
  }
}

case class Bishop(override val owner: String = WHITE, override val loc: Index = (-1, -1), override val moved: Boolean = false) extends Piece(owner, loc, moved) {

  override val id: String = "Bishop"

  override def copy: Bishop = Bishop(owner, loc, moved)

  override val location: String = iToA(loc)

  override def totalCoverage(implicit b: Board, mH: Gameplay): Indices = {
    val coverage: Indices = Array()
    for (x <- -SIDE until SIDE) {
      val locationA = (loc._1+x, loc._2+x) // main diagonal
      if (onBoard(locationA)) coverage:+locationA
      val locationB = (loc._1+x, loc._2-x) // perpendicular
      if (onBoard(locationB)) coverage:+locationB
    }
    val tC = coverage.distinct.filter(_ != loc)
    tC
  }

  override def accessible(implicit b: Board, mH: Gameplay): Indices = {
    val accessible = bishopAccess
    accessible
  }

  override def toString: String = pieceCase("B")
}

case class King(override val owner: String = WHITE, override val loc: Index = (-1, -1), override val moved: Boolean = false) extends Piece(owner, loc, moved) {

  override val id: String = "King"

  override def copy: King = King(owner, loc, moved)

  override val location: String = iToA(loc)

  override def totalCoverage(implicit b: Board, mH: Gameplay = ListBuffer()): Indices = {
    val tC = getCoverage(b)
    tC
  }

  def immediateCoverage(implicit b: Board): Indices = {
    (for (row <- downSelect(loc._1); col <- downSelect(loc._2)) yield (row, col)).filterNot(_ == loc)
  }

  private def getCoverage(b: Board): Indices = {
    var coverage = for (row <- downSelect(loc._1); col <- downSelect(loc._2)) yield (row, col)
    if (!moved) {
      coverage = if (castle(b, KS)) coverage :+ aToI(KSCOLUMN + (if (owner == WHITE) "1" else "8")) else coverage
      coverage = if (castle(b, QS)) coverage :+ aToI(QSCOLUMN + (if (owner == WHITE) "1" else "8")) else coverage
    }
    val tC = coverage.filterNot(_ == loc)
    tC
  }

  override def accessible(implicit b: Board, mH: Gameplay): Indices = {
    val tC = getCoverage(b)
    val accessible = tC.filterNot(b.forcePieceExists(_, owner))
    accessible
  }

  private def downSelect(index: Int): Array[Int] = {
    index match {
      case 0 => Array(index, index+1)
      case 7 => Array(index-1, index)
      case _ => Array(index-1, index, index+1)
    }
  }

  def check(implicit b: Board): Boolean = {
    val opponent = b.opponent(owner)
    val oP = b.pieces(opponent).filterNot(_.id == id)
    val checkTable = for (p <- oP) yield p.accessible(b).contains(loc)
    val checkStatus = checkTable.foldLeft(false)(_ || _)
    checkStatus || b.king(b.opponent(owner)).immediateCoverage(b).contains(loc)
  }

  def checkmate(implicit b: Board, mH: Gameplay): Boolean = if (check(b) && b.moves(owner, mH).isEmpty) true else false

  def castle(implicit b: Board, side: String): Boolean = {
    val col: String = side match {case KS => "H" case QS => "A"}
    val row: String = owner match {case WHITE => "1" case BLACK => "8"}
    val rookMoved: Boolean = b.piece(aToI(col+row)).getOrElse(None) match {
      case None => true
      case _ => b.piece(aToI(col+row)).get.moved
    }

    val cols: Array[String] = side match {case KS => Array("F", "G") case QS => Array("B", "C", "D")}

    def isPathClear: Boolean = {
      (for {column <- cols} yield b.piece(aToI(column+row)).getOrElse(None) match {
        case None => true
        case _ => false
      }).forall(x => x)
    }

    def isPathAttacked: Boolean = {
      (for {column <- cols} yield b.numberAttacking(b.opponent(owner), aToI(column+row)) == 0).forall(_ => true)
    }

    if (!moved && !rookMoved && !check(b) && isPathClear) {
      !isPathAttacked
    } else false
  }

  override def toString: String = pieceCase("K")
}

case class Knight(override val owner: String = WHITE, override val loc: Index = (-1, -1), override val moved: Boolean = false) extends Piece(owner, loc, moved) {

  override val id: String = "Knight"

  override def copy: Knight = Knight(owner, loc, moved)

  override val location: String = iToA(loc)

  override def totalCoverage(implicit b: Board, mH: Gameplay): Indices = {
    val tC = for (km <- knightMoves.filter(f => onBoard(f._1+loc._1, f._2+loc._2))) yield (loc._1+km._1, loc._2+km._2)
    tC
  }

  override def accessible(implicit b: Board, mH: Gameplay): Indices = {
    val accessible = (for (km <- knightMoves.filter(f => onBoard(f._1+loc._1, f._2+loc._2))) yield (loc._1+km._1, loc._2+km._2)).filterNot(b.forcePieceExists(_, owner))
    accessible
  }

  override def toString: String = pieceCase("N")
}

case class Pawn(override val owner: String = WHITE, override val loc: Index = (-1,-1), override val moved: Boolean = false) extends Piece(owner, loc, moved) {

  override val id: String = "Pawn"

  override def copy: Pawn = Pawn(owner, loc, moved)

  override val location: String = iToA(loc)

  override def totalCoverage(implicit b: Board, mH: Gameplay): Indices = { // exists as a wrapper in case trace should be added
    val tC = getCoverage
    tC
  }

  private def getCoverage(implicit b: Board): Indices = {
    val front = if (owner == WHITE) 1 else -1
    var tC: Indices = Array[Index]()
    tC = if (loc._1 != 7 || loc._1 != 0) {
      var temp = tC :+ (loc._1 + front, loc._2)
      temp = if (startingRow) temp :+ (loc._1 + (2*front), loc._2) else temp
      loc._2 match {
        case 0 => temp :+ (loc._1 + front, loc._2 + 1) // column "a"
        case 7 => temp :+ (loc._1 + front, loc._2 - 1) // column "h"
        case _ => temp :+ (loc._1 + front, loc._2 + 1) :+ (loc._1 + front, loc._2 - 1)
      }
    } else tC
    tC.filter(onBoard).filterNot(b.forcePieceExists(_, owner))
  }

  override def accessible(implicit b: Board, mH: Gameplay): Indices = {
    import Math._
    val front = if (owner == WHITE) 1 else -1
    var accessible: Indices = getCoverage
    val frontBlocked = b.pieceExists((loc._1+front, loc._2))
    val noDouble = frontBlocked || (onBoard((loc._1+2*front, loc._2)) && b.pieceExists((loc._1+2*front, loc._2)))
    for (index <- accessible) yield (abs(index._1-loc._1), abs(index._2-loc._2)) match {
      case (_,1) => accessible = if (b.pieceExists(index)) accessible else accessible.filterNot(_ == index) // attacking squares
      case (1,0) => accessible = if (frontBlocked) accessible.filterNot(_ == index) else accessible // double push
      case (2,0) => accessible = if (noDouble) accessible.filterNot(_ == index) else accessible // double push
      case (_,_) => accessible
    }
    accessible.filter(onBoard)
  }

  private def startingRow: Boolean = {
    val row = if (owner == WHITE) 1 else 6
    if ((owner == WHITE && loc._1 == row) || (owner == BLACK && loc._1 == row)) true else false
  }

  private def enPassant(implicit b: Board, mH: Gameplay): Option[Piece] = {
    import Math._
    val passingRow: Boolean = loc._1 == (if (owner == WHITE) 4 else 3)
    if (mH.nonEmpty) {
      val lastPiecePawn: Boolean = mH.last.piece.id == "Pawn"
      val adjacentColumn: Boolean = loc._2 == mH.last.piece.loc._2 - 1 || loc._2 == mH.last.piece.loc._2 + 1
      val lastPieceTwoSpace: Boolean = abs(mH.last.piece.loc._1 - mH.last.to._1) == 2
      if (passingRow && lastPiecePawn && adjacentColumn && lastPieceTwoSpace) {
        Some(mH.last.piece)
      } else None
    } else None
  }

  override def attack(implicit b: Board, mH: Gameplay): Pieces = {
    val front = if (owner == WHITE) 1 else -1
    val diags = Array((loc._1+front, loc._2+1), (loc._1+front, loc._2-1)).filter(onBoard)
    var attacking: Pieces = Array[Piece]()
    for (diag <- diags) {
      attacking = if (b.piece(diag).nonEmpty && (b.piece(diag).get.owner == b.opponent(owner))) {
        attacking :+ b.piece(diag).get
      } else attacking
    }
    if (enPassant(b, mH).nonEmpty) attacking :+ enPassant(b, mH).get else attacking
  }

  override def defend(implicit b: Board, mH: Gameplay): Pieces = {
    val front = if (owner == WHITE) 1 else -1
    val diags = Array((loc._1+front, loc._2+1), (loc._1+front, loc._2-1)).filter(onBoard)
    var defending: Pieces = Array[Piece]()
    for (diag <- diags) {
      defending = if (b.piece(diag).nonEmpty && (b.piece(diag).get.owner == owner)) {
        defending :+ b.piece(diag).get
      } else defending
    }
    defending
  }

  def promote: Boolean = loc._1 == (if (owner == WHITE) 7 else 0)

  override def toString: String = pieceCase("P")
}

case class Queen(override val owner: String = WHITE, override val loc: Index = (-1, -1), override val moved: Boolean = false) extends Piece(owner, loc, moved) {

  override val id: String = "Queen"

  override def copy: Queen = Queen(owner, loc, moved)

  override val location: String = iToA(loc)

  override def totalCoverage(implicit b: Board, mH: Gameplay): Indices = {

    val column =  for (x <- 0 until SIDE) yield (loc._1, x)
    val row = for (y <- 0 until SIDE) yield (y, loc._2)

    val diagonal = for (
      mod <- 1-SIDE until SIDE;
      x = loc._1 + mod;
      y = loc._2 + mod
      if onBoard((x, y))) yield (x, y)

    val off = for (
      mod <- 1-SIDE until SIDE;
      x = loc._1 + mod;
      y = loc._2 - mod
      if onBoard((x, y))) yield (x, y)
    
    val tC = column.++:(row).++:(off).++:(diagonal).distinct.filter(_ != loc).to[Array]
    tC
  }

  override def accessible(implicit b: Board, mH: Gameplay): Indices = {
    val accessible = rookAccess ++: bishopAccess
    accessible
  }

  override def toString: String = pieceCase("Q")
}

case class Rook(override val owner: String = WHITE, override val loc: Index = (-1, -1), override val moved: Boolean = false) extends Piece(owner, loc, moved) {

  override val id: String = "Rook"

  override def copy: Rook = Rook(owner, loc, moved)

  override val location: String = iToA(loc)

  override def totalCoverage(implicit b: Board, mH: Gameplay): Indices = {
    val column: Indices = (for (x <- 0 until SIDE) yield (loc._1, x)).to[Array]
    val row: Indices = (for (y <- 0 until SIDE) yield (y, loc._2)).to[Array]
    val tC: Indices = column.++:(row).distinct.filterNot(_ == loc)
    tC
  }

  override def accessible(implicit b: Board, mH: Gameplay): Indices = {
    val accessible = rookAccess
    accessible
  }

  override def toString: String = pieceCase("R")
}

object Piece {

  type Index = (Int, Int)
  type Indices = Array[Index]
  final val KS: String = "KingSide"
  final val QS: String = "QueenSide"
  final val KSCOLUMN: String = "G"
  final val QSCOLUMN: String = "C"
  final val knightMoves = Array((1, 2), (-1, 2), (1, -2), (-1, -2), (-2, 1), (-2, -1), (2, 1), (2, -1))

  def pieceInstance(piece: Piece): Piece = {
    piece.id match {
      case "King" => King(piece.owner, (piece.loc._1, piece.loc._2), piece.moved)
      case "Queen" => Queen(piece.owner, (piece.loc._1, piece.loc._2), piece.moved)
      case "Rook" => Rook(piece.owner, (piece.loc._1, piece.loc._2), piece.moved)
      case "Bishop" => Bishop(piece.owner, (piece.loc._1, piece.loc._2), piece.moved)
      case "Knight" => Knight(piece.owner, (piece.loc._1, piece.loc._2), piece.moved)
      case "Pawn" => Pawn(piece.owner, (piece.loc._1, piece.loc._2), piece.moved)
    }
  }

  def pieceInstance(id: String, owner: String, loc: Index, moved: Boolean): Piece = {
    id match {
      case "King" => King(owner, loc, moved)
      case "Queen" => Queen(owner, loc, moved)
      case "Rook" => Rook(owner, loc, moved)
      case "Bishop" => Bishop(owner, loc, moved)
      case "Knight" => Knight(owner, loc, moved)
      case "Pawn" => Pawn(owner, loc, moved)
    }
  }

}
