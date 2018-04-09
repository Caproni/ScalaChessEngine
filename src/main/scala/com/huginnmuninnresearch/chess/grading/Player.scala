package com.huginnmuninnresearch.chess.grading

case class Player(id: String, computer: Boolean = false) {

  var grade: Int = 1200

  override def toString: String = if (computer) id + " (C)" else id

}
