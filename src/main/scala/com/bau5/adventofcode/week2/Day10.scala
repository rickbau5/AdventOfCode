package com.bau5.adventofcode.week2

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/25/15.
  */
object Day10 extends Advent {
  def main(args: Array[String]) {
    val input = dayInput.head
    def lookAndSay(in: String): String = {
      val ret = in.drop(1).toList.foldLeft(("", in.head.toString)) { case ((all, current), char) =>
        if (char == current.head) {
          (all, current + char)
        } else {
          (all + s"${current.length}${current.head}", char.toString)
        }
      }
      ret._1 + ret._2.length + ret._2.head
    }

    var str = input
    for (i <- 0 until 50) str = lookAndSay(str)
    println(str.length)
  }
}
