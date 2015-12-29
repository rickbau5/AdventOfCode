package com.bau5.adventofcode.week1

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/18/15.
  */
object Day01 extends Advent {
  def main (args: Array[String]): Unit = {
    val input = dayInput
    println("Sum is " + sum(input.head))
    println("Basement at " + toBasement(input.head))
  }

  def sum(input: String) = input.foldLeft(0) { (sig, c) =>
    sig + { if (c == '(') 1 else -1 }
  }

  def toBasement(input: String) = {
    var flag = false
    var basement = 0
    input.foldLeft(0) { (sig, c) =>
      val n = sig + { if (c == '(') 1 else -1 }
      if (!flag) {
        basement += 1
        if (n == -1) {
          flag = true
        }
      }
      n
    }
    basement
  }
}
