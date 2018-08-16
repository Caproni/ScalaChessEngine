package com.huginnmuninnresearch.chess.engine

import Engine._
import breeze.linalg.min
import breeze.numerics.abs
import com.huginnmuninnresearch.chess.pieces.Piece
import com.huginnmuninnresearch.chess.state.Board.Pieces
import com.huginnmuninnresearch.chess.state.Moves.Gameplay
import com.huginnmuninnresearch.chess.state.{Board, Move, Moves}

import scala.collection.immutable.ListMap

class Engine { // eventually will pass constants in as parameters

  def bestMove(current: String, b: Board, mH: Gameplay): Move = {
    import Board._
    val possibleMoves = b.moves(current, mH)
    val depth: Int = 0
    val scores: Scores = evaluate(b, possibleMoves, mH)
    val newScores: Scores = if (inScope(depth+1) && !mate(b, current, mH)) {
      val searchScores: Scores = (for ((m, s) <- scores;
           vision: (Board, Moves.Gameplay) = future(b, m, mH);
           searchScore: Score = search(b.opponent(current), vision._1, mH, depth+1, m, s)
      ) yield scala.collection.mutable.Map(searchScore._1 -> searchScore._2)).head
      searchScores
    } else {
      scores
    }
    if (current == WHITE) {
      ListMap(newScores.toSeq.sortWith(_._2 > _._2):_*).head._1
    } else {
      ListMap(newScores.toSeq.sortWith(_._2 < _._2):_*).head._1
    }
  }

  private def mate(b: Board, owner: String, mH: Gameplay): Boolean = if (b.checkmate(owner, mH)) true else false

  private def future(b: Board, move: Move, mH: Gameplay): (Board, Gameplay) = {
    val fB = b.propose(move.piece, move.to, mH)
    val fMH = mH :+ Move(move.piece, move.to, b.piece(move.to), move.result)
    (fB, fMH)
  }

  private def evaluate(b: Board, move: Move, mH: Gameplay): Double = {
    import Board._
    val vision = future(b, move, mH)
    val result = score(WHITE, vision._1, vision._2) - score(BLACK, vision._1, vision._2)
    result
  }

  private def evaluate(b: Board, moves: Gameplay, mH: Gameplay): Scores = {
    val scoreMap: Scores = scala.collection.mutable.Map()
    for (move: Move <- moves) {
      scoreMap.+=((move, evaluate(b, move, mH)))
    }
    scoreMap
  }

  def score(owner: String, b: Board, mH: Gameplay): Double = {
    import Engine._
    checkmateWeight*checkmateScore(owner, b, mH) +
      materialWeight*materialScore(b.pieces(owner)) +
      totalCoverageWeight*coverageScore(owner, b, mH) +
      accessibleWeight*accessibleScore(owner, b, mH) +
      attackingWeight*attackingScore(owner, b, mH) +
      defendingWeight*defendingScore(owner, b, mH) +
      aggressionWeight*aggressionScore(owner, b, mH) +
      advancedPawnWeight*advancedPawnScore(owner, b, mH) +
      controlWeight*controlScore(owner, b, mH)
  }

  def inScope(depth: Int): Boolean = {
    import Engine._
    if (depth < maxSearchDepth) true else false
  }

  private def search(owner: String, b: Board, mH: Gameplay, depth: Int = 0, move: Move, score: Double): Score = {
    import scala.collection.immutable.ListMap
    val moves = b.moves(owner, mH)
    val moveScores: Scores = evaluate(b, moves, mH)
    val newScore: Scores = if (inScope(depth+1)) {
      val ns: Scores = (for (m <- ListMap(moveScores.toSeq.sortWith(_._2 > _._2):_*).take(powerMap(depth)).keys
        if !mate(b, owner, mH);
        vision: (Board, Moves.Gameplay) = future(b, m, mH);
        searchScores: Score = search(b.opponent(owner), vision._1, vision._2, depth+1, m, score)
      ) yield scala.collection.mutable.Map(searchScores._1 -> searchScores._2)).head
      ns
    } else { // maximum search depth reached
      moveScores
    }
    val orderedScores = ListMap(newScore.toSeq.sortWith(_._2 > _._2):_*)
    val updatedScore = immediateWeight*score + (1-immediateWeight)*(orderedScores.take(powerMap(depth)).values.sum / powerMap(depth))
    (move, updatedScore)
  }

  private def symmetricChecks(implicit b: Board, owner: String): Double = {
    val opportunities = for (piece <- b.pieces(owner)) yield Map(piece -> valueToTake(piece, b))
    opportunities.maxBy(_.values).values.head
  }

  private def checkmateScore(owner: String, b: Board, mH: Gameplay): Int = {
    if (b.checkmate(owner, mH)) 1 else 0
  }

  private def materialScore(pieces: Pieces): Double = {
    val scores = for (piece <- pieces) yield value(piece)
    scores.sum
  }

  private def coverageScore(owner: String, b: Board, mH: Gameplay): Double = {
    val coverages = for (piece <- b.pieces(owner)) yield piece.totalCoverage(b, mH).length
    coverages.sum
  }

  private def accessibleScore(owner: String, b: Board, mH: Gameplay): Int = {
    val accessible = for (piece <- b.pieces(owner)) yield piece.accessible(b, mH).length
    accessible.sum
  }

  private def controlScore(owner: String, b: Board, mH: Gameplay): Int = {
    val control = for (piece <- b.pieces(owner)) yield piece.defend(b, mH).length - piece.attack(b, mH).length
    control.sum
  }

  private def attackingScore(owner: String, b: Board, mH: Gameplay): Int = {
    val attacks = for (piece <- b.pieces(owner)) yield piece.attack(b, mH).length
    attacks.sum
  }

  private def defendingScore(owner: String, b: Board, mH: Gameplay): Int = {
    val defends = for (piece <- b.pieces(owner)) yield piece.defend(b, mH).length
    defends.sum
  }

  private def aggressionScore(owner: String, b: Board, mH: Gameplay): Int = {
    val enemyKing = b.king(b.opponent(owner)).loc
    val hammingDistances = for (piece <- b.pieces(owner).filterNot(_.id == "King")) yield abs(piece.loc._1 - enemyKing._1) + abs(piece.loc._2 - enemyKing._2)
    hammingDistances.sum
  }

  private def advancedPawnScore(owner: String, b: Board, mH: Gameplay): Int = {
    import Board._
    val pawns = b.pieces(owner).filter(_.id == "Pawn")
    val advancement = for (pawn <- pawns; if pawn.owner == owner) yield pawn.loc._1
    val scores = if (owner == "White") {
      for (row <- advancement) yield row*row
    } else {
      for (row <- advancement) yield (SIDE-row)*(SIDE-row)
    }
    scores.sum
  }

  private def valueToTake(piece: Piece, b: Board): Double = { // resolves attack chains
    val attackerValues = for (piece <- b.piecesAttacking(piece.owner, piece.loc)) yield value(piece)
    val defenderValues = for (piece <- b.piecesAttacking(b.opponent(piece.owner), piece.loc)) yield value(piece)
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

  type Scores = scala.collection.mutable.Map[Move, Double]
  type Score = (Move, Double)

  def apply: Engine = new Engine

  val immediateWeight: Double = 0.2
  val maxSearchDepth: Int = 3
  val powerMap = Map(1 -> 5, 2 -> 5, 3 -> 5, 4 -> 2, 5 -> 2)

  object PieceValue {
    val PawnValue: Double = 1.0
    val KnightValue: Double = 3.0
    val BishopValue: Double = 3.0
    val RookValue: Double = 5.0
    val QueenValue: Double = 9.0
    val KingValue: Double = 40.0
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
  val checkmateWeight: Double = 50.0
}
