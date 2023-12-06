package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Range
import utilities.Range.*

object Day05 extends AdventOfCode(Prod):
  val mappings: Vector[Set[Mapping]] =
    input
      .dropWhile(_ != '\n')
      .split("\n\n")
      .filter(_.nonEmpty)
      .map(Mapping.fromString)
      .toVector

  case class Mapping(destination: Long, source: Long, length: Long):
    lazy val sourceRange: Range = source until source + length - 1

    def map(range: Range): Option[Range] =
      range.intersect(sourceRange).map: (overlap: Range) =>
        overlap.min + (destination - source) to overlap.max + (destination - source)

  object Mapping:
    def fromString(mapping: String): Set[Mapping] =
      mapping
        .linesIterator
        .foldLeft(Set.empty[Mapping]):
          case (acc, s"$_ map:") => acc
          case (acc, s"$destination $source $length") =>
            acc + Mapping(destination.toLong, source.toLong, length.toLong)
          case (acc, _) => acc

  object Seeds:
    def fromString(seeds: String): Set[Range] =
      seeds match
        case s"seeds: $seeds" =>
          seeds
            .split(" ")
            .map((seed: String) => seed.toLong to seed.toLong + 1)
            .toSet

  object Ranges:
    def fromString(ranges: String): Set[Range] =
      ranges match
        case s"seeds: $ranges" =>
          ranges
            .split(" ")
            .grouped(2)
            .map: (range: Array[String]) =>
              val from   = range(0).toLong
              val length = range(1).toLong

              from until from + length
            .toSet

    def map(range: Range, mappings: Set[Mapping]): Set[Range] =
      val mapped: Set[Range] =
        mappings.flatMap((mapping: Mapping) => mapping.map(range))

      val unmapped: Set[Range] =
        mappings.foldLeft(Set(range)): (acc: Set[Range], mapping: Mapping) =>
          acc.flatMap((range: Range) => range.diff(mapping.sourceRange))

      mapped ++ unmapped

    extension (ranges: Set[Range])
      def mapAll(mappings: Vector[Set[Mapping]]): Set[Range] =
        mappings.foldLeft(ranges):
          (ranges: Set[Range], mapping: Set[Mapping]) =>
            ranges.flatMap(map(_, mapping))

      def lowestLocationNumber: Long =
        ranges
          .mapAll(mappings)
          .minBy(_.min)
          .min

  import Mapping.*
  import Ranges.*

  lazy val pt1: Long =
      Seeds
        .fromString(input.takeWhile(_ != '\n').trim)
        .lowestLocationNumber

  lazy val pt2: Long =
    Ranges
      .fromString(input.takeWhile(_ != '\n').trim)
      .lowestLocationNumber

  answer(1)(pt1)

  answer(2)(pt2)
