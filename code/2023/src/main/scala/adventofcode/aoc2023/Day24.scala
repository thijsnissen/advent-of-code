package adventofcode
package aoc2023

import adventofcode.utilities.Pos3D
import utilities.AdventOfCode.*

object Day24 extends AdventOfCode(Prod):
  val hailstones: Vector[Hailstone] =
    input
      .linesIterator
      .map(Hailstone.fromString)
      .toVector

  case class Hailstone(start: Pos3D, vector: Pos3D):
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
    def futureIntersection(that: Hailstone): Option[(Long, Long)] =
      def isFuture(px: Long, py: Long): Boolean =
        vector.x * (px - start.x) >= 0 && vector.y * (py - start.y) >= 0 &&
          that.vector.x * (px - that.start.x) >= 0 && that.vector.y * (py - that.start.y) >= 0

      // Line 1
      val x1: Long = start.x
      val y1: Long = start.y
      val x2: Long = start.x + vector.x
      val y2: Long = start.y + vector.y

      // Line 2
      val x3: Long = that.start.x
      val y3: Long = that.start.y
      val x4: Long = that.start.x + that.vector.x
      val y4: Long = that.start.y + that.vector.y

      val denominator: Long = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

      if denominator != 0 then
        val px: Long =
          ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) /
            denominator

        val py: Long =
          ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) /
            denominator

        Option.when(isFuture(px, py))(px -> py)
      else None

  object Hailstone:
    def fromString(s: String): Hailstone =
      s match
        case s"$x, $y, $z @ $vx, $vy, $vz" => Hailstone(
            Pos3D(x.trim.toLong, y.trim.toLong, z.trim.toLong),
            Pos3D(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
          )

    extension (self: Vector[Hailstone])
      def intersections(windowMin: Long, windowMax: Long): Int =
        self
          .combinations(2)
          .count:
            case Vector(a: Hailstone, b: Hailstone) =>
              a.futureIntersection(b) match
                case None => false
                case Some(px, py) =>
                  px >= windowMin && px <= windowMax &&
                    py >= windowMin && py <= windowMax
            case _ => sys.error("should never get here...")

  import Hailstone.*

  lazy val pt1: Int =
    val windowMin: Long = if getEnv == Test then 7 else 200000000000000L
    val windowMax: Long = if getEnv == Test then 27 else 400000000000000L

    hailstones.intersections(windowMin, windowMax)

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
