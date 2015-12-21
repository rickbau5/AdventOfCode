package com.bau5.adventofcode

/**
  * Created by Rick on 12/20/15.
  */
object Day08 {
  def main(args: Array[String]) {
    val input = getInput(getClass)
    val decoded = decode(input)
    val encoded = encode(input)

    println(decoded)
    println(encoded)
  }

  def encode(input: Seq[String]): Int = {
    input.foldLeft(0) { (sum, line) =>
      val all = line.length
      val encoded = line.tail.dropRight(1).foldLeft(0) { (sum, char) =>
        if (char == '\\' || char == '\"' ) {
          sum + 2
        } else {
          sum + 1
        }
      } + 6
      sum + (encoded - all)
    }
  }

  def decode(input: Seq[String]): Int = {
    var escaped = false
    input.foldLeft(0) { (sum,line) =>
      val all = line.length
      val mem = line.tail
        .dropRight(1)
        .foldLeft(0) { (sum, char) =>
          if (char == '\\' && !escaped) {
            escaped = true
            sum
          } else {
            if (escaped) {
              val r = if (char == 'x') {
                sum - 1
              } else {
                sum + 1
              }
              escaped = false
              r
            } else {
              sum + 1
            }
          }
        }
      sum + (all - mem)
    }
  }
}
