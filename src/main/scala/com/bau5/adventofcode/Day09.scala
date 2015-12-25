package com.bau5.adventofcode

/**
  * Created by Rick on 12/22/15.
  */
object Day09 {
  type Edge = ((City, City), Int)

  def main(args: Array[String]) {
    val regex = raw"(.+) to (.+) = (\d+)".r
    val input = getInput(getClass).map { case regex(c1, c2, dist) => (c1, c2) -> dist.toInt }.toMap
    val output = input.flatMap {
      case ((a, b), _) => Seq(a, b)
    }.toVector.distinct.permutations.map { path =>
      path.sliding(2).map {
        case Seq(a, b) => input.getOrElse((a, b), input(b, a))
      }.sum
    }.min
    //.max for max
    println(output)
  }
}

case class City(name: String) {
  override def toString: String = name
}