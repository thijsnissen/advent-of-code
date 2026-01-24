package adventofcode
package aoc2021

import utilities.AdventOfCode.*
import utilities.Utilities.*

object Day07 extends AdventOfCode(Prod):
  val submarines: Vector[Int] =
    input
      .filterNot(_.isControl)
      .split(",")
      .map(_.toInt)
      .toVector

  def cheapest(submarines: Vector[Int])(f: (Int, Int) => Int): Int =
    (submarines.min to submarines.max)
      .map(p => submarines.map(s => f(p, s)).sum)
      .min

  override lazy val pt1: Int =
    cheapest(submarines): (a, b) =>
      (a - b).abs

  override lazy val pt2: Int =
    cheapest(submarines): (a, b) =>
      (a - b).abs.triangular
