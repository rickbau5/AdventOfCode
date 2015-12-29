package com.bau5.adventofcode

import scala.io.Source

/**
  * Created by Rick on 12/28/15.
  */
trait Advent {
  lazy val dayInput = Source.fromFile(
      s"/Users/Rick/IdeaProjects/AdventOfCode/src/main/resources/input/${this.getClass.getSimpleName.replace("$", "")}.txt"
    ).getLines().toSeq
}
