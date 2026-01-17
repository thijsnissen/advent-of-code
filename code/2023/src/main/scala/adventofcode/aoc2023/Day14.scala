package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Cycle
import utilities.Utilities.sumBy

object Day14 extends AdventOfCode(Prod):
  val platform: Platform =
    input
      .linesIterator
      .toVector

  type Platform = Vector[String]

  object Platform:
    extension (self: Platform)
      def toggleLeftTiltable: Platform =
        self.transpose.map(_.mkString)

      def rotateClockwise: Platform =
        self.transpose.map(_.reverse.mkString)

      def tilt: Platform =
        self
          .toggleLeftTiltable
          .map: (row: String) =>
            row
              .split('#')
              .map(_.sorted.reverse)
              .mkString("#")
              .padTo(self.length, '#')
          .toggleLeftTiltable

      def cycle: Platform = (1 to 4).foldLeft(self)(
        (acc: Platform, _) => acc.tilt.rotateClockwise
      )

      def totalLoad: Int =
        self
          .zipWithIndex
          .sumBy: (row: String, y: Int) =>
            row.count(_ == 'O') * (self.length - y)

  import Platform.*

  lazy val pt1: Int =
    platform.tilt.totalLoad

  lazy val pt2: Int =
    val cycle: Cycle[Platform] =
      Cycle.find((p: Platform) => p.cycle, platform)(identity)

    val iterator: Iterator[Platform] =
      Iterator.iterate(platform)(_.cycle)

    iterator
      .drop:
        cycle.stemLength +
          ((1_000_000_000 - cycle.stemLength) % cycle.cycleLength)
      .next
      .totalLoad

  answer(1)(pt1)

  answer(2)(pt2)
