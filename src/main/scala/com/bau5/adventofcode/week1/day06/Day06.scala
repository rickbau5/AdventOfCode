package com.bau5.adventofcode.week1.day06

import com.bau5.adventofcode._
import com.bau5.adventofcode.week1.day06.Day06.Grid

import scala.collection.mutable


/**
  * Created by Rick on 12/19/15.
  */
object Day06 extends Advent {
  type Grid = mutable.Map[(Int, Int), Int]
  def main(args: Array[String]) {
    val statements = dayInput

    def calculate(lines: Seq[String])(func: (String, Grid) => Grid) =
      lines.foldLeft(mutable.Map.empty[(Int, Int), Int])((grid, statement) => func(statement, grid)).values.sum

    lazy val lightsOn = calculate(statements)(computeStatement_1)
    lazy val totalBrightness = calculate(statements)(computeStatement_2)

    println("Number of lights on: " + lightsOn)
    println("Total brightness is: " + totalBrightness)
  }

  // Part 1
  def computeStatement_1(string: String, arr: Grid): Grid = string match {
    case s if s.startsWith("turn on") => TurnOnStatement(s.substring("turn on ".length)).compute(arr)
    case s if s.startsWith("turn off") => TurnOffStatement(s.substring("turn off ".length)).compute(arr)
    case s if s.startsWith("toggle") => ToggleStatement(s.substring("toggle ".length)).compute(arr)
  }

  // Part 2
  def computeStatement_2(string: String, arr: Grid): Grid = string match {
    case s if s.startsWith("turn on") => IncreaseStatement(s.substring("turn on ".length)).compute(arr)
    case s if s.startsWith("turn off") => DecreaseStatement(s.substring("turn off ".length)).compute(arr)
    case s if s.startsWith("toggle") => IncreaseTwoStatement(s.substring("toggle ".length)).compute(arr)
  }
}

abstract class Statement(range: CoordinateRange) extends Operator {
  def compute(input: Grid): Grid = {
    range.getRange.foreach { case (x, r) =>
      r.foreach { y =>
        val in = input.get((x, y))
        if (in.isEmpty) {
          val res = op(0)
          if (res != 0) {
            input.put((x,y), res)
          }
        } else {
          input.put((x, y), op(in.get))
        }
      }
    }
    input
  }
}

case class TurnOnStatement(string: String)  extends Statement(CoordinateRange(string)) with On
case class TurnOffStatement(string: String) extends Statement(CoordinateRange(string)) with Off
case class ToggleStatement(string: String)  extends Statement(CoordinateRange(string)) with Toggle

case class IncreaseStatement(string: String)  extends Statement(CoordinateRange(string)) with Increase
case class DecreaseStatement(string: String) extends Statement(CoordinateRange(string)) with Decrease
case class IncreaseTwoStatement(string: String)  extends Statement(CoordinateRange(string)) with IncreaseTwo

case class CoordinateRange(c1: Coordinate, c2: Coordinate) {
  def getRange = Range(c1.x, c2.x).map(x => x -> Range(c1.y, c2.y))
}
object CoordinateRange {
  def apply(string: String): CoordinateRange = {
    val coords = string.filter(_ != ' ').split("through").map(str => Coordinate(str))
    CoordinateRange(coords.head, coords(1) + Coordinate(1, 1))
  }
}

case class Coordinate(x: Int, y: Int)  {
  def +(coordinate: Coordinate): Coordinate = {
    Coordinate(x + coordinate.x, y + coordinate.y)
  }
}
object Coordinate {
  def apply(string: String): Coordinate = {
    val coords = string.split(",").map(_.toInt)
    Coordinate(coords.head, coords(1))
  }
}

sealed trait Operator {
  def op(before: Int): Int
}

sealed trait On extends Operator {
  override def op(before: Int): Int = 1
}

sealed trait Off extends Operator {
  override def op(before: Int): Int = 0
}

sealed trait Toggle extends Operator {
  override def op(before: Int): Int = if (before == 0) 1 else 0
}

// Part 2
sealed trait Increase extends Operator {
  override def op(before: Int): Int = before + 1
}

sealed trait Decrease extends Operator {
  override def op(before: Int): Int = {
    val n = before - 1
    if (n < 0) {
      0
    } else {
      n
    }
  }
}

sealed trait IncreaseTwo extends Operator {
  override def op(before: Int): Int = before + 2
}
