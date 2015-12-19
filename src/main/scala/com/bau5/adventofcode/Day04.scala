package com.bau5.adventofcode

import java.security.MessageDigest


/**
  * Created by Rick on 12/18/15.
  */
object Day04 {
  def main(args: Array[String]): Unit = {
    val input = getInput(getClass).head
    val digestor = MessageDigest.getInstance("MD5")
    println("The number with \'00000\' prefix is " + findNumber("000000", digestor))
    println("The number with \'000000\' prefix is " + findNumber("0000000", digestor))
  }

  def findNumber(prefix: String, digestor: MessageDigest): Int = {
    var flag = false
    var number = 0
    (0 to Integer.MAX_VALUE).takeWhile { i =>
      if (getHash(prefix, i)(digestor).startsWith("00000")) {
        number = i
        flag = true
      }
      !flag
    }
    number
  }

  def getHash(key: String, num: Int)(digestor: MessageDigest): String = {
    digestor.digest(s"$key$num".getBytes).map("%02x".format(_)).mkString
  }
}
