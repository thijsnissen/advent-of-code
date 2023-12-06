package adventofcode
package aoc2023

import scala.collection.immutable.NumericRange
import utilities.AdventOfCode.*

object Day05 extends AdventOfCode(Test):
  val mappings: Vector[Set[Mapping]] =
    input
      .dropWhile(_ != '\n')
      .split("\n\n")
      .filter(_.nonEmpty)
      .map(Mapping.fromString)
      .toVector

  type Range = NumericRange[Long]
  type Seed  = Long

  case class Mapping(destination: Long, source: Long, length: Long)

  object Seed:
    def fromString(seeds: String): Vector[Long] =
      seeds match
        case s"seeds: $seeds" =>
          seeds
            .split(" ")
            .map(_.toLong)
            .toVector

    def map(seed: Seed, mapping: Set[Mapping]): Seed =
      mapping
        .flatMap:
          case Mapping(destination, source, length) =>
            Option.when((source until source + length).contains(seed)):
              seed + (destination - source)
        .headOption
        .getOrElse(seed)

    extension (seeds: Vector[Seed])
      def mapAll(mappings: Vector[Set[Mapping]]): Vector[Seed] =
        mappings.foldLeft(seeds):
          (seeds: Vector[Seed], mapping: Set[Mapping]) =>
            seeds.map(map(_, mapping))

  object Range:
    def fromString(ranges: String): Vector[Range] =
      ranges match
        case s"seeds: $ranges" =>
          ranges
            .split(" ")
            .grouped(2)
            .map: (range: Array[String]) =>
              val from   = range(0).toLong
              val length = range(1).toLong

              from until from + length
            .toVector

    def map(range: Range, mapping: Set[Mapping]): Set[Range] =
      mapping.flatMap:
        case Mapping(destination, source, length) =>
          range.split(source until source + length).map: (r: Range) =>
            if r.min >= source && r.max <= source + length then
              r.min + destination - source until r.max + destination - source
            else r

    extension (self: Range)
      def split(that: Range): Set[Range] =
        val overlap = (self.min max that.min) until (self.max min that.max)
        val above   = overlap.max until self.max
        val below   = self.min until overlap.min

        Set(overlap, above, below).filter(_.nonEmpty)

    extension (ranges: Vector[Range])
      def mapAll(mappings: Vector[Set[Mapping]]): Vector[Range] =
        mappings.foldLeft(ranges):
          (ranges: Vector[Range], mapping: Set[Mapping]) =>
            ranges.flatMap(map(_, mapping))

  object Mapping:
    def fromString(mapping: String): Set[Mapping] =
      mapping
        .linesIterator
        .foldLeft(Set.empty[Mapping]):
          case (acc, s"$_ map:") => acc
          case (acc, s"$destination $source $length") =>
            acc + Mapping(destination.toLong, source.toLong, length.toLong)
          case (acc, _) => acc

  import Mapping.*

  lazy val pt1: Long =
    import Seed.*

    val seeds: Vector[Seed] =
      Seed.fromString(input.takeWhile(_ != '\n').trim)

    seeds.mapAll(mappings).min

  lazy val pt2: Long =
    import Range.*

    val ranges: Vector[Range] =
      Range.fromString(input.takeWhile(_ != '\n').trim)

    ranges
      .mapAll(mappings)
      .map(_.min)
      .min

  answer(1)(pt1)

  answer(2)(pt2)
