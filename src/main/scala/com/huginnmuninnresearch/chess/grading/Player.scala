package com.huginnmuninnresearch.chess.grading

case class Player(id: String, computer: Boolean = false) {

  var grade: Int = 1200

  override def toString: String = if (computer) s"$id (C)" else s"$id ($grade)"

}
