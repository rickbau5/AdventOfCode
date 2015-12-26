package com.bau5.adventofcode

/**
  * Created by Rick on 12/25/15.
  */
object Day11 {
  val bad = List('i', 'o', 'l')

  def main(args: Array[String]): Unit = {
    val input = getInput(getClass).head

    def find(string: String): String = validPassword(string) match {
      case false => find(increment(string))
      case true => string
    }
    val first = find(input)
    println("The first password is " + first)
    println("The second password is " + find(increment(first)))
  }

  def validPassword(string: String): Boolean =
    !(bad.exists(e => string.contains(s"$e")) || !increasing(string) || !doubles(string))

  def increment(string: String): String = {
    var add = true
    string.foldRight("") { (char, string) =>
      val c = if (add) {
        if (char + 1 > 'z') {
          add = true
          'a'
        } else {
          add = false
          (char + 1).toChar
        }
      } else {
        char
      }
      c + string
    }
  }

  def increasing(string: String): Boolean =
    string.sliding(3)
      .exists { e =>
        e(1) - e(0) == 1 && e(2) - e(1) == 1
      }

  def doubles(string: String): Boolean =
    string.sliding(2)
      .filter(e => e(0) == e(1))
      .toList
      .distinct
      .size >= 2
}
