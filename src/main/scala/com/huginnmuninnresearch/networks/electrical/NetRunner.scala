package com.huginnmuninnresearch.networks.electrical

import scala.collection.mutable.ListBuffer

class NetRunner(start: Double, stop: Double, timestep: Double) {

  val nodesNo = 4
  val nodes: ListBuffer[Node] = ListBuffer[Node]()

  for (n <- 1 to nodesNo) {
    val generation = if (n < 3) 1 else 0
    val consumption = if (n < 3) 0 else 1
    nodes.append(Node(n.toString, generation, consumption))
  }
  nodes(0).links.append(DiodeLink("1A", 0.5, nodes(1)))
  nodes(1).links.append(DiodeLink("2A", 0.4, nodes(2)))
  nodes(2).links.append(DiodeLink("3A", 0.3, nodes(3)))

  val net = Net("Alpha", nodes)

  val times = Array(start to stop by timestep : _*)
  for (time <- times) {
    println(net.update)
  }

}
