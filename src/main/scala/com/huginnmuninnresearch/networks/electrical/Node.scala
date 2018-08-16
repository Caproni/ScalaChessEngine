package com.huginnmuninnresearch.networks.electrical

import breeze.linalg.min

import scala.collection.mutable.ListBuffer

class Node(val nodeID: String, val generation: Double, val consumption: Double) {

  val links: ListBuffer[DiodeLink] = ListBuffer[DiodeLink]()

  def netOutput: Double = generation - consumption

  def connected: ListBuffer[Node] = for (l <- links) yield l.toNode

  def countLinks: Int = links.length

  def maxTransmission(l: DiodeLink): Double = min(l.capacity, netOutput)

}

object Node {
  def apply(nodeID: String, generation: Double, consumption: Double) = new Node(nodeID, generation, consumption)
}