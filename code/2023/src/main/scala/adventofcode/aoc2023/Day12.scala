package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day12 extends AdventOfCode(Test):
  val conditionRecords: Vector[Record] =
    input
      .linesIterator
      .map(Record.fromString)
      .toVector

  case class Record(springs: String, groups: List[Int]):
    def arrangements: List[String] =
      @tailrec def loop(i: Int, g: String, acc: String): String =
        if i >= springs.length then acc
        else
          springs(i) match
            case '.' | '?' if g.isEmpty => loop(i + 1, g, acc + ".")
            case '.' | '?' if g.nonEmpty && groups.contains(g.length) =>
              loop(i + 1, "", acc + g)
            case '#' | '?' => loop(i + 1, g + "#", acc)

      loop(0, "", "") :: List.empty[String]

  object Record:
    def fromString(s: String): Record =
      val Array(springs, groups) = s.split(" ")

      Record(springs, groups.split(",").map(_.toInt).toList)

  lazy val pt1: Int =
    conditionRecords
      .flatMap(_.arrangements)
      .tap(x => pprint.log(x))
      .length

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
