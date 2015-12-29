package com.bau5.adventofcode.week2

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/25/15.
  */
object Day12 extends Advent {
  def main(args: Array[String]) {
    val input = dayInput.head

    val ret = input.toCharArray.foldLeft((List.empty[Int], "")) { case ((numbers, current), char) =>
      if (char.isDigit || char == '-') {
        val curr = current + char
        (numbers, curr)
      } else {
        val add = if (current != "") {
          List(current.toInt)
        } else {
          List.empty
        }
        (numbers ++ add, "")
      }
    }._1.sum
    println(ret)
  }
}
