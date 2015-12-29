package com.bau5.adventofcode.week1

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/18/15.
  */
object Day02 extends Advent {
  def main(args: Array[String]) {
    val input = dayInput
    println("Wrapping paper needed: " + calculatePaperUsed(input))
    println("Wrapping paper needed: " + calculateRibbonNeeded(input))
  }

  def calculatePaperUsed(input: Seq[String]): Int = {
    input.foldLeft(0) { (sig, str) =>
      var i = 0
      val dims = str.split("x").map(_.toInt)
      val mult = dims.map { dim =>
        val inner = i match {
          case 0 => dim * dims(1)
          case 1 => dim * dims(2)
          case 2 => dim * dims.head
        }
        i += 1
        inner
      }

      mult.foldLeft(0) { (sum, num) =>
        sum + num * 2
      } + mult.min + sig
    }
  }

  def calculateRibbonNeeded(input: Seq[String]): Int = {
    input.foldLeft(0) { (sig, str) =>
      val dims = str.split("x").map(_.toInt)
      val max = dims.max
      var flag = false
      val rest = dims.filter { e =>
        if (e == max && !flag) {
          flag = true
          false
        } else {
          true
        }
      }
      sig + rest.foldLeft(0) {(s, i) => s + i * 2} + dims.product
    }
  }
}


