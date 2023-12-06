package adventofcode
package aoc2023

import scala.collection.immutable.NumericRange
import utilities.AdventOfCode.*

object Day05 extends AdventOfCode(Prod):
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
    def fromString(ranges: String): Vector[Set[Range]] =
      ranges match
        case s"seeds: $ranges" =>
          ranges
            .split(" ")
            .grouped(2)
            .map: (range: Array[String]) =>
              val from   = range(0).toLong
              val length = range(1).toLong

              Set(from until from + length)
            .toVector

    def map(range: Set[Range], mapping: Set[Mapping]): Set[Range] =
      mapping
        .flatMap:
          case Mapping(destination, source, length) =>
            Some(range) // apply mapping to Set[Range] -> return new Set[Range]
        .headOption
        .getOrElse(range)

    extension (ranges: Vector[Set[Range]])
      def mapAll(mappings: Vector[Set[Mapping]]): Vector[Set[Range]] =
        mappings.foldLeft(ranges):
          (ranges: Vector[Set[Range]], mapping: Set[Mapping]) =>
            ranges.map(map(_, mapping))

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

    val ranges: Vector[Set[Range]] =
      Range.fromString(input.takeWhile(_ != '\n').trim)

    ranges
      .mapAll(mappings)
      .flatten
      .minBy(_.min)
      .min

  answer(1)(pt1)

  answer(2)(pt2)
