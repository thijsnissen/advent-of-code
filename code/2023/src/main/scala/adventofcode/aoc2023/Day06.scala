package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day06 extends AdventOfCode(Prod):
  val races: Vector[Race] =
    Race.fromInput(input.linesIterator.toVector):
      _.split(" ").filter(_.nonEmpty).map(_.toLong).toVector

  val race: Race =
    Race
      .fromInput(input.linesIterator.toVector): (s: String) =>
        Vector(s.replace(" ", "").toLong)
      .head

  case class Race(time: Long, distance: Long):
    def race: Long =
      @tailrec def loop(i: Long, acc: Long): Long =
        i match
          case 0                              => acc
          case i if i * (time - i) > distance => loop(i - 1, acc + 1)
          case _                              => loop(i - 1, acc)

      loop(time, 0)

  object Race:
    def fromInput(lines: Vector[String])(transformation: String => Vector[Long])
      : Vector[Race] =
      val Vector(times: Vector[Long], distances: Vector[Long]) =
        lines.map:
          case s"Time: $times"         => transformation(times)
          case s"Distance: $distances" => transformation(distances)

      times.zip(distances).map(Race.apply)

  override lazy val pt1: Long =
    races
      .map(_.race)
      .product

  override lazy val pt2: Long =
    race.race
