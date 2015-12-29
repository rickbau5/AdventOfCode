package com.bau5.adventofcode.week3

import com.bau5.adventofcode.Advent

/**
  * Created by Rick on 12/28/15.
  */
object Day16 extends Advent {
  def main(args: Array[String]): Unit = {
    val regex = raw"Sue (\d+): (.+): (\d+), (.+): (\d+), (.+): (\d+)".r
    val r = raw"(.+): (\d+)".r
    val correctSue = AnAuntSue(0, correctSueTraits.split(", ").map {
      case r(_trait, _num) => _trait -> _num.toInt
    }.toMap.withDefaultValue(0))

    val sues = dayInput.takeWhile {
      case regex(id, trait1, num1, trait2, num2, trait3, num3) =>
        correctSue != AnAuntSue(id.toInt, trait1, num1.toInt, trait2, num2.toInt, trait3, num3.toInt)
    }.toList
    println(sues.size + 1)
  }

  val correctSueTraits =
    """children: 3
      |cats: 7
      |samoyeds: 2
      |pomeranians: 3
      |akitas: 0
      |vizslas: 0
      |goldfish: 5
      |trees: 3
      |cars: 2
      |perfumes: 1""".stripMargin.replace("\n", ", ")
}

case class AnAuntSue(id: Int, traits: Map[String, Int]) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case AnAuntSue(otherId, otherTraits) =>
      otherTraits.toList.forall {
        // Part 2
        case ct if ct._1 == "cats" || ct._1 == "trees" => traits(ct._1) < otherTraits(ct._1)
        case pg if pg._1 == "pomeranians" || pg._1 == "goldfish" => traits(pg._1) > otherTraits(pg._1)
        // End Part 2
        case e => traits.toList.contains(e)

      }
    case _ => super.equals(obj)
  }
}
object AnAuntSue {
  def apply(id: Int, trait1: String, num1: Int, trait2: String, num2: Int, trait3: String, num3: Int): AnAuntSue = {
    AnAuntSue(id, List((trait1, num1), (trait2, num2), (trait3, num3)).toMap.withDefaultValue(0))
  }
}