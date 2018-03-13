package com.huginnmuninnresearch.chess.state

object Winner extends Enumeration {
  val White: Value = Value(1)
  val Black: Value = Value(-1)
  val Draw: Value = Value(0)
}
