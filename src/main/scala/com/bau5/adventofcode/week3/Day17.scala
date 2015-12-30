package com.bau5.adventofcode.week3

import com.bau5.adventofcode.Advent

/**
  * Created by Rick on 12/28/15.
  */
object Day17 extends Advent {
  def main(args: Array[String]): Unit = {
    val input = dayInput.toList.zipWithIndex.map(c => Container(c._2, c._1.toInt))

    def combine(n: Int, l: List[Container]): List[List[Int]] =
      l.combinations(n)
        .map(_.map(_.capacity))
        .toList

    val ret = (1 to input.size).map(combine(_, input).count(_.sum == 150)).sum
    val min = (1 to input.size).find(combine(_, input).count(_.sum == 150) > 0)
    println(ret)
    println(min)
  }
}

case class Container(idx: Int, capacity: Int)
