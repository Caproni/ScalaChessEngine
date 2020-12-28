package com.huginnmuninnresearch.chess.engine

import breeze.linalg.min
import breeze.numerics.abs
import com.huginnmuninnresearch.chess.engine.Engine._
import com.huginnmuninnresearch.chess.pieces.Piece
import com.huginnmuninnresearch.chess.record.MoveHistory
import com.huginnmuninnresearch.chess.state.Board.{Pieces, WHITE}
import com.huginnmuninnresearch.chess.state.{Board, Move}

import java.text.DecimalFormat
import scala.collection.immutable.ListMap

class Engine() { // eventually will pass constants in as parameters

  def bestMove(currentPlayer: String, b: Board, mH: MoveHistory): Move = {
    val currentDepth: Int = 0
    val scores: Scores = evaluate(b, currentPlayer, mH)
    val newScores: Scores = if (inScope(currentDepth + 1) && !mate(b, currentPlayer, mH)) {
      val searchScores: Iterable[Scores] = for (
        (m, s) <- scores;
        vision: (Board, MoveHistory) = future(b, m, mH);
        searchScore: Score = search(b.opponent(currentPlayer), vision._1, vision._2, currentDepth + 1, m, s)
      ) yield ListMap(searchScore._1 -> searchScore._2)
      var flatSearchScores: Scores = ListMap()
      for (x <- searchScores) {
        flatSearchScores = flatSearchScores ++ x
      }
      flatSearchScores
    } else {
      scores
    }

    val sortedScores = sortScores(currentPlayer, newScores)
    printScores(currentPlayer, mH, sortedScores)
    sortedScores.head._1
  }

  private def sortScores(currentPlayer: String, scores: Scores): Scores = {
    val sortedScores = if (currentPlayer == WHITE) {
      ListMap(scores.toSeq.sortWith(_._2 > _._2):_*)
    } else {
      ListMap(scores.toSeq.sortWith(_._2 < _._2):_*)
    }
    sortedScores
  }

  private def mate(b: Board, owner: String, mH: MoveHistory): Boolean = if (b.checkmate(owner, mH)) true else false

  private def future(b: Board, move: Move, mH: MoveHistory): (Board, MoveHistory) = {
    val fB = b.propose(move.piece, move.to, mH)
    val fMH: MoveHistory = mH.copy()
    fMH.addMove(Move(move.piece, move.to, b.piece(move.to), move.result))
    (fB, fMH)
  }

  private def evaluate(b: Board, move: Move, mH: MoveHistory): Double = {
    import Board._
    val vision = future(b, move, mH)
    val result = score(WHITE, vision._1, vision._2) - score(BLACK, vision._1, vision._2)
    result
  }

  private def evaluate(b: Board, owner: String, mH: MoveHistory): Scores = {
    val moves = b.moves(owner, mH)
    var scoreMap = ListMap[Move, Double]()
    for (move: Move <- moves) {
      scoreMap.+=((move, evaluate(b, move, mH)))
    }
    scoreMap
  }

  def score(owner: String, b: Board, mH: MoveHistory): Double = {
    import Engine._
    checkmateWeight * checkmateScore(owner, b, mH) +
      materialWeight * materialScore(b.pieces(owner)) +
      totalCoverageWeight * coverageScore(owner, b, mH) +
      accessibleWeight * accessibleScore(owner, b, mH) +
      attackingWeight * attackingScore(owner, b, mH) +
      defendingWeight * defendingScore(owner, b, mH) +
      aggressionWeight * aggressionScore(owner, b, mH) +
      advancedPawnWeight * advancedPawnScore(owner, b, mH) +
      controlWeight * controlScore(owner, b, mH)
  }

  def inScope(depth: Int): Boolean = {
    import Engine._
    if (depth < maxSearchDepth) true else false
  }

  private def search(owner: String, b: Board, mH: MoveHistory, depth: Int = 0, move: Move, score: Double): Score = {
    val moveScores = sortScores(owner, evaluate(b, owner, mH))
    val newScore = if (inScope(depth + 1)) {
      val ns: Iterable[Scores] = for (
        (m, s) <- moveScores
        if !mate(b, owner, mH);
        vision: (Board, MoveHistory) = future(b, m, mH);
        searchScores: Score = search(b.opponent(owner), vision._1, vision._2, depth + 1, m, s)
      ) yield ListMap(searchScores._1 -> searchScores._2)
      if (ns.isEmpty) ListMap[Move, Double]() else ns.head
    } else { // maximum search depth reached
      moveScores
    }
    val sortedScores = sortScores(owner, newScore)
//    if (depth == 1) printScores(owner, move, sortedScores)
    val updatedScore = immediateWeight * score + (1 - immediateWeight) * evaluationScore(owner, depth, sortedScores)
//    if (depth == 1) println(s"Selected move: $move has updated score: $updatedScore (original score: $score)")
    (move, updatedScore)
  }

  private def symmetricChecks(implicit b: Board, owner: String, mH: MoveHistory): Double = {
    val opportunities = for (piece <- b.pieces(owner)) yield ListMap(piece -> valueToTake(piece, b, mH))
    opportunities.maxBy(_.values).values.head
  }

  private def checkmateScore(owner: String, b: Board, mH: MoveHistory): Int = {
    if (b.checkmate(owner, mH)) 1 else 0
  }

  private def materialScore(pieces: Pieces): Double = {
    val scores = for (piece <- pieces) yield value(piece)
    scores.sum
  }

  private def coverageScore(owner: String, b: Board, mH: MoveHistory): Double = {
    val coverages = for (piece <- b.pieces(owner)) yield piece.totalCoverage(b, mH).length
    coverages.sum
  }

  private def accessibleScore(owner: String, b: Board, mH: MoveHistory): Int = {
    val accessible = for (piece <- b.pieces(owner)) yield piece.accessible(b, mH).length
    accessible.sum
  }

  private def controlScore(owner: String, b: Board, mH: MoveHistory): Int = {
    val control = for (piece <- b.pieces(owner)) yield piece.defend(b, mH).length - piece.attack(b, mH).length
    control.sum
  }

  private def attackingScore(owner: String, b: Board, mH: MoveHistory): Int = {
    val attacks = for (piece <- b.pieces(owner)) yield piece.attack(b, mH).length
    attacks.sum
  }

  private def defendingScore(owner: String, b: Board, mH: MoveHistory): Int = {
    val defends = for (piece <- b.pieces(owner)) yield piece.defend(b, mH).length
    defends.sum
  }

  private def aggressionScore(owner: String, b: Board, mH: MoveHistory): Int = {
    val enemyKing = b.king(b.opponent(owner)).loc
    val hammingDistances = for (piece <- b.pieces(owner).filterNot(_.id == "King")) yield abs(piece.loc._1 - enemyKing._1) + abs(piece.loc._2 - enemyKing._2)
    hammingDistances.sum
  }

  private def advancedPawnScore(owner: String, b: Board, mH: MoveHistory): Int = {
    import Board._
    val pawns = b.pieces(owner).filter(_.id == "Pawn")
    val advancement = for (pawn <- pawns; if pawn.owner == owner) yield pawn.loc._1
    val scores = if (owner == "White") {
      for (row <- advancement) yield row * row
    } else {
      for (row <- advancement) yield (SIDE - row) * (SIDE - row)
    }
    scores.sum
  }

  private def valueToTake(piece: Piece, b: Board, mH: MoveHistory): Double = { // resolves attack chains
    val attackerValues = for (piece <- b.piecesAttacking(piece.owner, piece.loc, mH)) yield value(piece)
    val defenderValues = for (piece <- b.piecesAttacking(b.opponent(piece.owner), piece.loc, mH)) yield value(piece)
    val noAttackers = attackerValues.length
    val noDefenders = defenderValues.length
    val scores = for (e <- 0 until min(noDefenders, noAttackers)) yield (defenderValues(e) - attackerValues(e)) + value(piece)
    if (scores.nonEmpty && scores.exists(_ > 0)) scores.filter(_ > 0).head else 0
  }

  private def value(piece: Piece): Double = {
    import Engine.PieceValue._
    piece.id match {
      case "Pawn" => PawnValue
      case "Knight" => KnightValue
      case "Bishop" => BishopValue
      case "Rook" => RookValue
      case "Queen" => QueenValue
      case "King" => KingValue
    }
  }

}

object Engine {

  type Score = (Move, Double)
  type Scores = ListMap[Move, Double]

  def apply: Engine = new Engine

  val immediateWeight: Double = 0.1
  val maxSearchDepth: Int = 4
  val powerMap = Map(1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1)

  private def evaluationScore(owner: String, depth: Int, sortedScores: Scores): Double = {
    val toTake = min(3, sortedScores.keys.toArray.length)
    var sum: Double = 0.0
    if (owner == WHITE) {
      sortedScores.toSeq.sortWith(_._2 > _._2).take(toTake).foreach(sum += _._2)
    } else sortedScores.toSeq.sortWith(_._2 < _._2).take(toTake).foreach(sum += _._2)
    sum / toTake
  }

  private def printScoresInner(scores: Scores): Unit = {
    val df = new DecimalFormat("####0.00")
    val h = "============================"
    println(h)
    println("| Move           | Score   |")
    println(h)
    for (score <- scores) {
      val adjMove = for (_ <- score._1.toString().length - 14 to 0) yield " "
      val adjScore = if (score._2 < 0) "" else " "
      println(s"| ${score._1}${adjMove.mkString}| $adjScore${df.format(score._2)}   | ")
    }
    println(s"$h\n")
  }

  def printScores(owner: String, move: Move, scores: Scores): Unit = {
    println(s"Options for $owner given previous move: $move:\n")
    printScoresInner(scores)
  }

  def printScores(owner: String, mH: MoveHistory, scores: Scores): Unit = {
    println(s"Options for $owner given previous move: ${if (mH.m.moves.nonEmpty) mH.last else '-'}:\n")
    printScoresInner(scores)
  }

  object PieceValue {
    val PawnValue: Double = 1.0
    val KnightValue: Double = 3.0
    val BishopValue: Double = 3.0
    val RookValue: Double = 5.0
    val QueenValue: Double = 9.0
    val KingValue: Double = 100.0
  }

  val totalCoverageWeight: Double = 0.1
  val accessibleWeight: Double = 0.5
  val attackingWeight: Double = 0.1
  val defendingWeight: Double = 0.00
  val materialWeight: Double = 3
  val aggressionWeight: Double = 0.01
  val controlWeight: Double = 0.3
  val defendedPiecesWeight: Double = 0.6
  val attackedPiecesWeight: Double = 0.6
  val advancedPawnWeight: Double = 0.0
  val checkmateWeight: Double = 100.0
}
