package com.huginnmuninnresearch.chess.grading

trait GameResult extends Enumeration {
  val Win: Value = Value(1)
  val Draw: Value = Value(0)
  val Lose: Value = Value(-1)
}
