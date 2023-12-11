package adventofcode
package aoc2018

import utilities.AdventOfCode.*
import utilities.Pos4D

object Day25 extends AdventOfCode(Test):
  val fixedPointsInSpacetime: Vector[Pos4D] =
    input
      .linesIterator
      .map:
        case s"$x,$y,$z,$w" => Pos4D(x.toInt, y.toInt, z.toInt, w.toInt)
      .toVector

  def numberOfConstellations(fixedPointsInSpacetime: Vector[Pos4D]): Map[Pos4D, Set[Pos4D]] =
    ???

  lazy val pt1: Int =
    numberOfConstellations(fixedPointsInSpacetime).size

  answer(1)(pt1)
