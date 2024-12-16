package adventofcode
package aoc2024

import utilities.AdventOfCode.*

object Day13 extends AdventOfCode(Prod):
  val clawMachines: Vector[Machine] =
    input
      .linesIterator
      .filterNot(_.isEmpty)
      .grouped(3)
      .map(Machine.fromSeq)
      .toVector

  case class Machine(
    ax: Long,
    ay: Long,
    bx: Long,
    by: Long,
    px: Long,
    py: Long
  ):
    lazy val fix: Machine =
      copy(px = px + 10000000000000L, py = py + 10000000000000L)

    lazy val win: Option[Long] =
      // ax * a + bx * b = px
      // ay * a + by * b = py

      // ax * by * a + bx * by * b = px * by
      // ay * bx * a + bx * by * b = py * bx --

      // ax * by * a - ay * bx * a = px * by - py * bx
      // (ax * by - ay * bx) * a = px * by - py * bx
      // a = (px * by - py * bx) / (ax * by - ay * bx)

      // ax * a + bx * b = px
      // bx * b = px - ax * a
      // b = (px - ax * a) / bx

      val a1 = px * by - py * bx
      val a2 = ax * by - ay * bx
      val a  = a1 / a2

      val b1 = px - ax * a
      val b2 = bx
      val b  = b1 / b2

      Option.when(a1 % a2 == 0 && b1 % b2 == 0)(a * 3 + b)

  object Machine:
    def fromSeq(s: Seq[String]): Machine =
      val poss = s.map:
        case s"Button A: X+$x, Y+$y" => (x = x.toLong, y = y.toLong)
        case s"Button B: X+$x, Y+$y" => (x = x.toLong, y = y.toLong)
        case s"Prize: X=$x, Y=$y"    => (x = x.toLong, y = y.toLong)

      Machine(poss(0).x, poss(0).y, poss(1).x, poss(1).y, poss(2).x, poss(2).y)

  lazy val pt1: Long =
    clawMachines
      .flatMap(_.win)
      .sum

  lazy val pt2: Long =
    clawMachines
      .flatMap(_.fix.win)
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
