package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val calories: Vector[Vector[Int]] =
    input
      .linesIterator
      .foldLeft(Vector(Vector.empty[Int])):
        case (acc, "")  => Vector.empty[Int] +: acc
        case (acc, cal) => acc.updated(0, cal.toInt +: acc(0))

  lazy val pt1: Int =
    calories
      .map(_.sum)
      .max

  lazy val pt2: Int =
    calories
      .map(_.sum)
      .sorted
      .takeRight(3)
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
