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

    def arrangements(springs: String, groups: List[Int]): Long =
      cache.getOrElseUpdate((springs, groups),
        if groups.isEmpty && springs.contains('#') then 0
        else if groups.isEmpty then 1
        else springs.headOption match
          case Some('.') => arrangements(springs.tail, groups)
          case Some('?') => arrangements("." + springs.tail, groups) + arrangements("#" + springs.tail, groups)
          case Some('#') =>
            if springs.length < groups.head then 0
            else if springs.take(groups.head).contains('.') then 0
            else if springs.slice(groups.head, groups.head + 1) == "#" then 0
            else arrangements(springs.drop(groups.head + 1), groups.tail)
          case _ => 0
      )

    extension (self: Vector[Record])
      def sumArrangements: Long =
        self
          .map((r: Record) => arrangements(r.springs, r.groups))
          .sum

  lazy val pt1: Long =
    val result = conditionRecords.sumArrangements
    println(s"CACHE POST PART 1")
    println(Record.cache.toList.sortBy(_._1._1.reverse).mkString("\n"))
    result

  lazy val pt2: Long =
    val result = conditionRecords.map(_.unfold).sumArrangements
    println(s"CACHE POST PART 2")
    println(Record.cache.toList.sortBy(_._1._1.reverse).mkString("\n"))
    result

  answer(1)(pt1)
  answer(2)(pt2)
