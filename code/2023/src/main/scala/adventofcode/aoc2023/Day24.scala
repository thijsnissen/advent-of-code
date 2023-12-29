package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day24 extends AdventOfCode(Prod):
  val hailstones: Vector[Hailstone] =
    input
      .linesIterator
      .map(Hailstone.fromString)
      .toVector

  case class Hailstone(start: Pos, vector: Pos):
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
    def futureIntersection(that: Hailstone): Option[(BigDecimal, BigDecimal)] =
      def isFuture(px: BigDecimal, py: BigDecimal): Boolean =
        (px - start.x).sign == vector.x.sign && (px - that.start.x).sign == that.vector.x.sign &&
          (py - start.y).sign == vector.y.sign && (py - that.start.y).sign == that.vector.y.sign

      // Line 1
      val Pos(x1, y1, _) = start
      val Pos(x2, y2, _) = start + vector

      // Line 2
      val Pos(x3, y3, _) = that.start
      val Pos(x4, y4, _) = that.start + that.vector

      val denominator: BigDecimal =
        (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

      if denominator == 0 then None
      else
        val px: BigDecimal =
          ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) /
            denominator

        val py: BigDecimal =
          ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) /
            denominator

        Option.when(isFuture(px, py))(px -> py)

  object Hailstone:
    def fromString(s: String): Hailstone =
      s match
        case s"$sx, $sy, $sz @ $vx, $vy, $vz" => Hailstone(
            Pos(BigDecimal(sx.trim), BigDecimal(sy.trim), BigDecimal(sz.trim)),
            Pos(BigDecimal(vx.trim), BigDecimal(vy.trim), BigDecimal(vz.trim))
          )

    extension (self: Vector[Hailstone])
      def intersections(windowMin: Long, windowMax: Long): Int =
        self
          .combinations(2)
          .count:
            case Vector(a: Hailstone, b: Hailstone) =>
              a futureIntersection b match
                case None => false
                case Some(px, py) =>
                  px >= windowMin && px <= windowMax &&
                  py >= windowMin && py <= windowMax
            case _ => sys.error("should never get here...")

  case class Pos(x: BigDecimal, y: BigDecimal, z: BigDecimal):
    @targetName("addition")
    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y, z + that.z)

    @targetName("subtraction")
    def -(that: Pos): Pos =
      Pos(x - that.x, y - that.y, z - that.z)

    @targetName("product")
    def *(i: Int): Pos =
      Pos(x * i, y * i, z * i)

  import Hailstone.*

  lazy val pt1: Int =
    val windowMin: Long = if getEnv == Test then 7 else 200000000000000L
    val windowMax: Long = if getEnv == Test then 27 else 400000000000000L

    hailstones.intersections(windowMin, windowMax)

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
