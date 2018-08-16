package com.huginnmuninnresearch.networks.electrical

import scala.collection.mutable.ListBuffer

class Net(val netID: String, val nodes: ListBuffer[Node] = ListBuffer[Node]()) {

  def update: Unit = {
    for (node <- nodes) {
      if (node.netOutput != 0 && node.countLinks != 0) { // unbalanced node
        for (l <- node.links) { // need to check connected nodes

        }
      }
    }
  }

  override def toString: String = {
    val nodal = for (node <- nodes) yield node.toString
    nodal.toString
  }

  def netOutput: Double = {
    val outputs = for (node <- nodes) yield node.netOutput
    outputs.sum
  }

  def totalGeneration: Double = {
    val outputs = for (node <- nodes) yield node.generation
    outputs.sum
  }

  def totalConsumption: Double = {
    val outputs = for (node <- nodes) yield node.consumption
    outputs.sum
  }

  def countLinks: Int = {
    val outputs = for (node <- nodes) yield node.links.length
    outputs.sum
  }

  def countNodes: Int = nodes.length

}

object Net {
  def apply(netID: String, nodes: ListBuffer[Node]): Net = new Net(netID, nodes)
}
