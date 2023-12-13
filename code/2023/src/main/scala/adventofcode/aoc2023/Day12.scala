package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day12 extends AdventOfCode(Test):
  val conditionRecords =
    input
      .linesIterator
      .map(Record.fromString)
      .toVector

  case class Record(springs: String, groups: List[Int])

  object Record:
    def fromString(s: String): Record =
      val record = s.split(" ")

      Record(record(0), record(1).split(",").map(_.toInt).toList)

  lazy val pt1: Int =
    ???

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
