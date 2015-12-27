package com.bau5.adventofcode

/**
  * Created by Rick on 12/25/15.
  */
object Day13 {
  def main(args: Array[String]) {
    val regex = raw"(.+) would (.+) (.+) happiness units by sitting next to (.+).".r
    val mapped = getInput(getClass).map {
      case regex(name, disposition, units, neighbor) =>
        (name, neighbor) -> units.toInt * { if (disposition == "gain") 1 else -1 }
    }.toMap
    val guests = mapped.keys.flatMap { case (a, b) => Seq(a, b) }.toList

    def findBest(people: List[String], happiness: Map[(String, String), Int]): Int = people.permutations
      .map { combos =>
        combos.sliding(2)
          .map { arrangement =>
            happiness.get((arrangement.head, arrangement.last)).get + happiness.get((arrangement.last, arrangement.head)).get
          }.sum + happiness.get(combos.last, combos.head).get + happiness.get(combos.head, combos.last).get
      }.max
    println(findBest(guests, mapped))
    println(findBest(guests ++ List("You"), mapped ++ guests.distinct.flatMap(e => List(("You", e) -> 0, (e, "You") -> 0))))
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
