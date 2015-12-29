package com.bau5.adventofcode.week2

import com.bau5.adventofcode._

/**
  * Created by Rick on 12/25/15.
  */
object Day13 extends Advent {
  def main(args: Array[String]) {
    val regex = raw"(.+) would (.+) (.+) happiness units by sitting next to (.+).".r
    val mapped = dayInput.map {
      case regex(name, disposition, units, neighbor) =>
        (name, neighbor) -> units.toInt * { if (disposition == "gain") 1 else -1 }
    }.toMap.withDefaultValue(0)
    val guests = mapped.keys.flatMap { case (a, b) => Seq(a, b) }.toList

    def findBest(people: List[String]): Int = people.permutations
      .map { combos =>
        combos.sliding(2)
          .map { arrangement =>
            mapped(arrangement.head, arrangement.last) + mapped(arrangement.last, arrangement.head)
          }.sum + mapped(combos.last, combos.head) + mapped(combos.head, combos.last)
      }.max
    println(findBest(guests))
    println(findBest("You" :: guests))
  }

  val inputTest =
    s"""Alice would gain 54 happiness units by sitting next to Bob.
       |Alice would lose 79 happiness units by sitting next to Carol.
       |Alice would lose 2 happiness units by sitting next to David.
       |Bob would gain 83 happiness units by sitting next to Alice.
       |Bob would lose 7 happiness units by sitting next to Carol.
       |Bob would lose 63 happiness units by sitting next to David.
       |Carol would lose 62 happiness units by sitting next to Alice.
       |Carol would gain 60 happiness units by sitting next to Bob.
       |Carol would gain 55 happiness units by sitting next to David.
       |David would gain 46 happiness units by sitting next to Alice.
       |David would lose 7 happiness units by sitting next to Bob.
       |David would gain 41 happiness units by sitting next to Carol."""
      .stripMargin.split("\n").toList
}
