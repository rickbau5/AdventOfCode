package com.bau5

import scala.io.Source

/**
  * Created by Rick on 12/18/15.
  */
package object adventofcode {
  def getInput[T](c: Class[T]): Seq[String] = {
    Source.fromFile(s"/Users/Rick/IdeaProjects/AdventOfCode/src/main/resources/input/${c.getSimpleName.replace("$", "")}.txt")
      .getLines()
      .toSeq
  }
}
