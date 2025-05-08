package adventofcode
package aoc2017

import utilities.AdventOfCode.*

object Day02 extends AdventOfCode(Prod):
  val spreadsheet: Vector[Vector[Int]] =
    input
      .linesIterator
      .map(_.split("\\s+").map(_.toInt).toVector)
      .toVector

  lazy val pt1: Long =
    spreadsheet.foldLeft(0): (acc, s) =>
      acc + s.max - s.min

  lazy val pt2: Long =
    spreadsheet
      .flatMap(_.combinations(2))
      .foldLeft(0):
        case (acc, Vector(a, b)) if a % b == 0 => acc + a / b
        case (acc, Vector(a, b)) if b % a == 0 => acc + b / a
        case (acc, _) => acc

  answer(1)(pt1)

  answer(2)(pt2)
