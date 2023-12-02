package adventofcode
package aoc2023

import scala.util.matching.Regex
import utilities.AdventOfCode.*

object Day02 extends AdventOfCode(Prod):
  val games: Vector[Game] =
    input
      .linesIterator
      .map(Game.fromString)
      .toVector

  case class Game(id: Int, reveals: Vector[Cube]):
    def isPossible(red: Int, green: Int, blue: Int): Boolean =
      reveals.forall:
        case Cube.Red(count)   => count <= red
        case Cube.Green(count) => count <= green
        case Cube.Blue(count)  => count <= blue

    def powerOfMinimumSetOfCubes: Int =
      reveals
        .groupMapReduce(_.ordinal)(_.getCount)(_ max _)
        .values
        .product

  object Game:
    def fromString(s: String): Game =
      s match
        case s"Game $id: $games" =>
          Game(
            id.toInt,
            revealsPattern
              .findAllIn(games)
              .toVector
              .map(Cube.fromString),
          )

    lazy val revealsPattern: Regex = """(\d+ \w+)+""".r

  enum Cube:
    case Red(count: Int)
    case Green(count: Int)
    case Blue(count: Int)

  object Cube:
    def fromString(s: String): Cube =
      s match
        case s"$i red"   => Cube.Red(i.toInt)
        case s"$i green" => Cube.Green(i.toInt)
        case s"$i blue"  => Cube.Blue(i.toInt)

    extension (c: Cube)
      def getCount: Int =
        c match
          case Red(count)   => count
          case Green(count) => count
          case Blue(count)  => count

  lazy val pt1: Int =
    games
      .filter(_.isPossible(red = 12, green = 13, blue = 14))
      .map(_.id)
      .sum

  lazy val pt2: Int =
    games
      .map(_.powerOfMinimumSetOfCubes)
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
