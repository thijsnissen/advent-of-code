package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val calibrationDocument: Vector[String] =
    input
      .linesIterator
      .toVector

  val digits: Map[String, String] =
    Map(
      "one"   -> "o1e",
      "two"   -> "t2o",
      "three" -> "t3e",
      "four"  -> "f4r",
      "five"  -> "f5e",
      "six"   -> "s6x",
      "seven" -> "s7n",
      "eight" -> "e8t",
      "nine"  -> "n9e",
    )

  @tailrec
  def firstValidDigit(line: String): Int =
    if line.head.isDigit then line.head.asDigit else firstValidDigit(line.tail)

  def normalize(line: String): String =
    digits.foldLeft(line):
      case (acc, (a, b)) => acc.replaceAll(a, b)

  def calibrationValue(line: String): Int =
    s"${firstValidDigit(line)}${firstValidDigit(line.reverse)}".toInt

  lazy val pt1: Int =
    calibrationDocument
      .map(calibrationValue)
      .sum

  lazy val pt2: Int =
    calibrationDocument
      .map(normalize)
      .map(calibrationValue)
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
