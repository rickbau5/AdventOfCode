package com.bau5.adventofcode.week2

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/26/15.
  */
object Day14 extends Advent {
  def main(args: Array[String]) {
    val regex = raw"(.+) can fly (.+) km/s for (.+) seconds, but then must rest for (.+) seconds.".r

    val speeds = dayInput.map {
      case regex(name, speed, active, inactive) => name -> (speed.toInt, active.toInt, inactive.toInt)
    }.toMap

    val time = 2503
    println(partOne(speeds, time))
  }

  def partOne(speeds: Map[String, (Int, Int, Int)], time: Int): Int = {
    speeds.map { case (deer, (speed, active, inactive)) =>
      val cycle = active + inactive
      val times = time / cycle
      val left = time % cycle
      val extra = if (left > active) active else left

      (speed * active) * times + extra * speed
    }.max
  }
}
