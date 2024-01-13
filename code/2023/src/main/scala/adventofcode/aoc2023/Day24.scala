package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day24 extends AdventOfCode(Prod):
  val hailstones: Vector[Hailstone3D] =
    input
      .linesIterator
      .map(Hailstone3D.fromString)
      .toVector

  case class Pos2D(x: BigDecimal, y: BigDecimal):
    @targetName("addition")
    def +(that: Pos2D): Pos2D =
      Pos2D(x + that.x, y + that.y)

    @targetName("subtraction")
    def -(that: Pos2D): Pos2D =
      Pos2D(x - that.x, y - that.y)

  case class Pos3D(x: BigDecimal, y: BigDecimal, z: BigDecimal):
    @targetName("addition")
    def +(that: Pos3D): Pos3D =
      Pos3D(x + that.x, y + that.y, z + that.z)

    @targetName("subtraction")
    def -(that: Pos3D): Pos3D =
      Pos3D(x - that.x, y - that.y, z - that.z)

  enum Axis:
    case X, Y, Z

  case class Rock(start: Pos3D, velocity: Pos3D)

  case class Hailstone2D(start: Pos2D, velocity: Pos2D):
    def reframe(v: Pos2D): Hailstone2D =
      copy(velocity = velocity - v)

    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
    def futureIntersection(that: Hailstone2D): Option[Pos2D] =
      def isFuture(px: BigDecimal, py: BigDecimal): Boolean =
        (px - start.x).sign == velocity.x.sign &&
          (py - start.y).sign == velocity.y.sign &&
          (px - that.start.x).sign == that.velocity.x.sign &&
          (py - that.start.y).sign == that.velocity.y.sign

      // Line 1
      val Pos2D(x1, y1) = start
      val Pos2D(x2, y2) = start + velocity

      // Line 2
      val Pos2D(x3, y3) = that.start
      val Pos2D(x4, y4) = that.start + that.velocity

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

        Option.when(isFuture(px, py))(Pos2D(px, py))

  case class Hailstone3D(start: Pos3D, velocity: Pos3D):
    def project(axis: Axis): Hailstone2D =
      axis match
        case Axis.X =>
          Hailstone2D(Pos2D(start.y, start.z), Pos2D(velocity.y, velocity.z))
        case Axis.Y =>
          Hailstone2D(Pos2D(start.x, start.z), Pos2D(velocity.x, velocity.z))
        case Axis.Z =>
          Hailstone2D(Pos2D(start.x, start.y), Pos2D(velocity.x, velocity.y))

  object Hailstone3D:
    def fromString(s: String): Hailstone3D =
      s match
        case s"$sx, $sy, $sz @ $vx, $vy, $vz" => Hailstone3D(
            Pos3D(sx.trim.toLong, sy.trim.toLong, sz.trim.toLong),
            Pos3D(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
          )

    extension (self: Vector[Hailstone3D])
      def project(axis: Axis): Vector[Hailstone2D] =
        self.map(_.project(axis))

  object Hailstone2D:
    extension (self: Vector[Hailstone2D])
      def intersections(windowMin: Long, windowMax: Long): Int =
        self
          .combinations(2)
          .count: (hailstones: Vector[Hailstone2D]) =>
            hailstones(0) futureIntersection hailstones(1) match
              case None => false
              case Some(Pos2D(px, py)) =>
                px >= windowMin && px <= windowMax &&
                py >= windowMin && py <= windowMax

      def findRock: Rock =
        val velocities: Seq[Pos2D] =
          for
            x <- self.map(_.velocity.x).min.toLong to self.map(
              _.velocity.x
            ).max.toLong
            y <- self.map(_.velocity.y).min.toLong to self.map(
              _.velocity.y
            ).max.toLong
          yield Pos2D(x, y)

        @tailrec def loop(
          stones: Vector[Hailstone2D],
          v: Pos2D,
          prev: Option[Pos2D],
          acc: Vector[Pos2D]
        ): Vector[Pos2D] =
          stones.headOption match
            case None => acc
            case Some(stone) =>
              stone.reframe(v) futureIntersection self.head.reframe(v) match
                case None => acc
                case Some(p) if prev.isEmpty =>
                  loop(stones.tail, v, Some(p), acc)
                case Some(p) if p == prev =>
                  loop(stones.tail, v, Some(p), acc :+ p)
                case Some(_) => acc

        val test =
          velocities.flatMap: v =>
            loop(self.tail, v, None, Vector.empty[Pos2D])

        Rock(Pos3D(0, 0, 0), Pos3D(0, 0, 0))

  import Hailstone2D.*

  lazy val pt1: Int =
    val windowMin: Long = if getEnv == Test then 7 else 200000000000000L
    val windowMax: Long = if getEnv == Test then 27 else 400000000000000L

    hailstones.project(Axis.Z).intersections(windowMin, windowMax)

  lazy val pt2: BigDecimal =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
