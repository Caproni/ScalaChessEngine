package com.huginnmuninnresearch.networks.electrical

class DiodeLink(val linkID: String, val capacity: Double, val toNode: Node) {

}

object DiodeLink {
  def apply(linkID: String, capacity: Double, toNode: Node): DiodeLink = new DiodeLink(linkID, capacity, toNode)
}
