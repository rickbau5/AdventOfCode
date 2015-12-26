package com.bau5.adventofcode

/**
  * Created by Rick on 12/25/15.
  */
object Day12 {
  def main(args: Array[String]) {
    val input = getInput(getClass).head

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
