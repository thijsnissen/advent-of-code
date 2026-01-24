package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Cache

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

    def arrangements: Long =
      Record.cache(springs, groups)

  object Record:
    def fromString(s: String): Record =
      val Array(springs, groups) = s.split(" ")

      Record(springs, groups.split(",").map(_.toInt).toList)

    lazy val cache: (String, List[Int]) => Long =
      Cache.memoize[String, List[Int], Long]:
        (springs: String, groups: List[Int]) =>
          if groups.isEmpty && !springs.contains('#') then 1
          else
            springs.headOption match
              case Some('.') => cache(springs.tail, groups)
              case Some('?') =>
                cache("." + springs.tail, groups) +
                  cache("#" + springs.tail, groups)
              case Some('#') =>
                groups.headOption match
                  case Some(g) if springs.length < g             => 0
                  case Some(g) if springs.take(g).contains('.')  => 0
                  case Some(g) if springs.slice(g, g + 1) == "#" => 0
                  case Some(g) => cache(springs.drop(g + 1), groups.tail)
                  case None    => 0
              case _ => 0

    extension (self: Vector[Record])
      def sumArrangements: Long =
        self
          .map(_.arrangements)
          .sum

  override lazy val pt1: Long =
    conditionRecords.sumArrangements

  override lazy val pt2: Long =
    conditionRecords
      .map(_.unfold)
      .sumArrangements
