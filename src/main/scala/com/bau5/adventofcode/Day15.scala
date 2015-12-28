package com.bau5.adventofcode

/**
  * Created by Rick on 12/27/15.
  */
object Day15 {

  def main(args: Array[String]): Unit = {
    val regex = raw"(.+): capacity (.+), durability (.+), flavor (.+), texture (.+), calories (.+)".r
    val ingredients = getInput(getClass).map {
      case regex(ingred, cap, dur, flavor, tex, cal) => List(cap.toInt, dur.toInt, flavor.toInt, tex.toInt, cal.toInt)
    }
    var max = 0
    for (a <- 0 until 100) {
      for (b <- 0 until 100 if a + b < 100) {
        for (c <- 0 until 100 if a + b + c < 100) {
          for (d <- 0 until 100 if a + b + c + d == 100) {
            val adjusted = List(a, b, c, d).zip(ingredients)
              .map(o => o._2.map(_ * o._1))
            val mult = zip4(adjusted.head, adjusted(1), adjusted(2), adjusted.last)
              .map { case (e, f, g, h) => List(0, e + f + g + h).max }
              .dropRight(1)
              .product
            max = List(max, mult).max
          }
        }
      }
    }

    println(max)
  }

  def zip4[A, B, C, D](l1: List[A], l2: List[B], l3: List[C], l4: List[D]): List[(A, B, C, D)] = {
    if (l1.size != l2.size && l1.size != l3.size && l1.size != l4.size) {
      throw new IllegalArgumentException("Lists must be the same length")
    }
    l1 match {
      case Nil => List.empty
      case last :: Nil => List((last, l2.last, l3.last, l4.last))
      case head :: tail => (head, l2.head, l3.head, l4.head) :: zip4(tail, l2.tail, l3.tail, l4.tail)
    }
  }
}
