package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Pos
import utilities.Utilities.+%

object Day14 extends AdventOfCode(Prod):
  val robots: Vector[Robot] =
    input
      .linesIterator
      .map(Robot.fromString)
      .toVector

  case class Robot(p: Pos, v: Pos):
    def simulate(width: Int, height: Int): Robot =
      copy(p = Pos((p.x + v.x) +% width, (p.y + v.y) +% height))

  object Robot:
    def fromString(s: String): Robot =
      s match
        case s"p=$px,$py v=$vx,$vy" =>
          Robot(Pos(px.toInt, py.toInt), Pos(vx.toInt, vy.toInt))

    extension (self: Vector[Robot])
      def simulate(width: Int, height: Int): Vector[Robot] =
        self.map(_.simulate(width, height))

      def christmasTree(width: Int, height: Int): Int = (1 to width * height)
        .foldLeft((second = 0, safetyFactor = Int.MaxValue, state = self)):
          (acc, s) =>
            val ns  = acc.state.simulate(width, height)
            val nsf = ns.safetyFactor(width, height)

            if nsf < acc.safetyFactor then (s, nsf, ns)
            else (acc.second, acc.safetyFactor, ns)
        .second

      def safetyFactor(width: Int, height: Int): Int =
        val (mx, my) = (width / 2, height / 2)

        val (a, b, c, d) =
          self.foldLeft((
            Set.empty[Robot],
            Set.empty[Robot],
            Set.empty[Robot],
            Set.empty[Robot]
          )):
            case ((a, b, c, d), r) =>
              if r.p.x < mx && r.p.y < my then (a + r, b, c, d)
              else if r.p.x > mx && r.p.y < my then (a, b + r, c, d)
              else if r.p.x < mx && r.p.y > my then (a, b, c + r, d)
              else if r.p.x > mx && r.p.y > my then (a, b, c, d + r)
              else (a, b, c, d)

        a.size * b.size * c.size * d.size

  import Robot.*

  override lazy val pt1: Int =
    val width: Int  = if getEnv == Test then 11 else 101
    val height: Int = if getEnv == Test then 7 else 103

    Iterator
      .iterate(robots)(_.simulate(width, height))
      .drop(100)
      .next()
      .safetyFactor(width, height)

  override lazy val pt2: Int =
    robots.christmasTree(
      width = if getEnv == Test then 11 else 101,
      height = if getEnv == Test then 7 else 103
    )
