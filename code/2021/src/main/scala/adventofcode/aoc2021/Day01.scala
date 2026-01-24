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

  override lazy val pt1: Int =
    countDepthMeasurementIncreases(sonarSweepReport)

  override lazy val pt2: Int =
    val depths: Vector[Int] =
      sonarSweepReport
        .sliding(3)
        .map(_.sum)
        .toVector

    countDepthMeasurementIncreases(depths)
