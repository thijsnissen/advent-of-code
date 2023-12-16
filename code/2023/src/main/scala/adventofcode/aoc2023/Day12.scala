package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day12 extends AdventOfCode(Prod):
  val conditionRecords: Vector[Record] =
    input
      .linesIterator
      .map(Record.fromString)
      .toVector

  case class Record(springs: String, groups: List[Int]):
    def unfold: Record =
      Record(
        List.fill(5)(springs).mkString("?"),
        List.fill(5)(groups).flatten
      )

  object Record:
    def fromString(s: String): Record =
      val Array(springs, groups) = s.split(" ")

      Record(springs, groups.split(",").map(_.toInt).toList)

    val cache = scala.collection.mutable.Map.empty[(String, List[Int]), Long]

    def arrangements(mask: String, lengths: List[Int]): Long =
      cache.getOrElseUpdate((mask, lengths),
        if lengths.isEmpty && mask.contains('#') then 0
        else if lengths.isEmpty then 1
        else mask.headOption match
          case Some('.') => arrangements(mask.tail, lengths)
          case Some('?') => arrangements("." + mask.tail, lengths) + arrangements("#" + mask.tail, lengths)
          case Some('#') =>
            if mask.length < lengths.head then 0
            else if mask.take(lengths.head).contains('.') then 0
            else if mask.slice(lengths.head, lengths.head + 1) == "#" then 0
            else arrangements(mask.drop(lengths.head + 1), lengths.tail)
          case _ => 0
      )

    extension (self: Vector[Record])
      def sumArrangements: Long =
        self
          .map((r: Record) => arrangements(r.springs, r.groups))
          .sum

  lazy val pt1: Long =
    conditionRecords.sumArrangements

  lazy val pt2: Long =
    conditionRecords
      .map(_.unfold)
      .sumArrangements

  answer(1)(pt1)

  answer(2)(pt2)
