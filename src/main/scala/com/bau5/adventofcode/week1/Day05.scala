package com.bau5.adventofcode.week1

import com.bau5.adventofcode._

import scala.util.Try

/**
  * Created by Rick on 12/18/15.
  */
object Day05 extends Advent {
  def main(args: Array[String]) {
    val input = dayInput

    def checkNiceness(in: Seq[String])(func: (String) => Boolean): Int = {
      input.foldLeft(0) { (sig, string) =>
        if (func(string)) {
          sig + 1
        } else {
          sig
        }
      }
    }

    println("Number of nice strings is " + checkNiceness(input)(isNice))
    println("Number of nicer strings is " + checkNiceness(input)(isNicer))
  }

  def isNicer(string: String): Boolean = {
    val slices = string.sliding(2)
      .toSeq
      .distinct

    val hasPairs = slices.exists { str =>
      val first = string.indexOf(str)
      val sub = Try(string.drop(first + 2))
        .toOption
        .map(_.indexOf(str))
      val yes = sub.get != -1
      sub.isDefined && yes
    }

    var i = 0
    var hasSplit = false
    string.drop(2).takeWhile { c =>
      if (c == string(i)) {
        hasSplit = true
      }
      i += 1
      !hasSplit
    }

    hasSplit && hasPairs
  }

  def isNice(string: String): Boolean = {
    val vowels = string.foldLeft(0) { (sig, char) =>
      if (isVowel(char)) {
        sig + 1
      } else {
        sig
      }
    }

    var doubleChar = false
    var last = '\0'
    string.takeWhile { char =>
      if (last == char) {
        doubleChar = true
      } else {
        last = char
      }
      !doubleChar
    }

    val noNaughtyStrings = List("ab", "cd", "pq", "xy")
      .map(string.contains)
      .forall(_ == false)

    vowels >= 3 && doubleChar && noNaughtyStrings
  }

  def isVowel(char: Char): Boolean = char match {
    case 'a' | 'e' | 'i' | 'o' | 'u' => true
    case _ => false
  }
}
