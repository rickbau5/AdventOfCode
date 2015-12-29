package com.bau5.adventofcode.week1

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/18/15.
  */
object Day03 extends Advent {
  type Pos = (Int, Int)
  def main(args: Array[String]) {
    val input = dayInput.head

    println(s"Visited ${santaNavigate(input, (0,0), Seq((0,0))).size} houses")
    println(s"Visited ${santaAndRoboNavigate(input, ((0,0), (0,0)), Seq((0,0))).size} houses")
  }

  def santaNavigate(string: String, last: Pos, positions: Seq[Pos]): Seq[Pos] = {
    val newPos = navigate(string.head, last)
    val seq = if (!positions.contains(newPos)) {
      positions ++ Seq(newPos)
    } else {
      positions
    }
    if (string.tail.isEmpty) {
      seq
    } else {
      santaNavigate(string.tail, newPos, seq)
    }
  }

  def santaAndRoboNavigate(string: String, last: (Pos, Pos), positions: Seq[Pos]): Seq[Pos] = {
    val newPoses = (navigate(string.head, last._1), navigate(string.tail.head, last._2))
    def add(pos: Pos, list: Seq[Pos]): Seq[Pos] = {
      if (!list.contains(pos)) {
        list ++ Seq(pos)
      } else {
        list
      }
    }

    val seq = add(newPoses._1, add(newPoses._2, positions))
    val next = string.drop(2)
    if (next.isEmpty) {
      seq
    } else {
      santaAndRoboNavigate(next, newPoses, seq)
    }
  }

  def navigate(char: Char, last: Pos): Pos = char match {
    case '^' => (last._1, last._2 + 1)
    case '>' => (last._1 + 1, last._2)
    case '<' => (last._1 - 1, last._2)
    case 'v' => (last._1, last._2 - 1)
  }
}
