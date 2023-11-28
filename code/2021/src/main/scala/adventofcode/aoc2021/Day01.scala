package adventofcode
package aoc2021

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val sonarSweepReport: Vector[Int] =
    input
      .linesIterator
      .map(_.toInt)
      .toVector

  def countDepthMeasurementIncreases(depths: Vector[Int]): Int =
    depths
      .sliding(2)
      .count(depths => depths(1) > depths(0))

  lazy val pt1: Int =
    countDepthMeasurementIncreases(sonarSweepReport)

  lazy val pt2: Int =
    val depths: Vector[Int] =
      sonarSweepReport
        .sliding(3)
        .map(_.sum)
        .toVector

    countDepthMeasurementIncreases(depths)

  answer(1)(pt1)

  answer(2)(pt2)
