package com.huginnmuninnresearch.fileread

import scala.io.Source

class TxtRead {

  val bufferedSource = Source.fromFile("data.txt")
  for (line <- bufferedSource.getLines) {
    println(line.trim)
  }

  bufferedSource.close

}

object TxtRead {
  def main(args: Array[String]): Unit = {

  }
}
