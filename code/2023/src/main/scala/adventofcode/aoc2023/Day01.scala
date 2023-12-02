package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val calibrationDocument: Vector[String] =
    input
      .linesIterator
      .toVector

  lazy val lettersToDigit: Map[String, Int] =
    Map(
      "one"   -> 1,
      "two"   -> 2,
      "three" -> 3,
      "four"  -> 4,
      "five"  -> 5,
      "six"   -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine"  -> 9,
    )

  @tailrec
  def firstValidDigit(line: String): Int =
    if line.head.isDigit then line.head.asDigit else firstValidDigit(line.tail)

  def calibrationValueFromDigits(line: String): Int =
    s"${firstValidDigit(line)}${firstValidDigit(line.reverse)}".toInt

  def calibrationValueFromDigitsLetters(line: String): Int =
    s"${startsWith(line)}${endsWith(line)}".toInt

  @tailrec
  def startsWith(line: String): Int =
    if line.head.isDigit then line.head.asDigit
    else
      lettersToDigit.find((letters, _) => line.startsWith(letters)) match
        case Some(_, digit) => digit
        case None           => startsWith(line.tail)

  @tailrec
  def endsWith(line: String): Int =
    if line.last.isDigit then line.last.asDigit
    else
      lettersToDigit.find((letters, _) => line.endsWith(letters)) match
        case Some(_, digit) => digit
        case None           => endsWith(line.init)

  lazy val pt1: Int =
    calibrationDocument
      .map(calibrationValueFromDigits)
      .sum

  lazy val pt2: Int =
    calibrationDocument
      .map(calibrationValueFromDigitsLetters)
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
