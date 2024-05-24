package adventofcode
package aoc2021

import utilities.AdventOfCode.*
import utilities.Pos

object Day05 extends AdventOfCode(Prod):
  val lines: Vector[Line] =
    input
      .linesIterator
      .map(Line.fromString)
      .toVector

  case class Line(from: Pos, to: Pos):
    def isDiagonal: Boolean =
      from.x != to.x && from.y != to.y

    def points: Vector[Pos] =
      (for
        x <- (from.x min to.x) to (from.x max to.x)
        y <- (from.y min to.y) to (from.y max to.y)
      yield Pos(x, y)).toVector

  object Line:
    def fromString(s: String): Line =
      s match
        case s"$fx,$fy -> $tx,$ty" =>
          Line(Pos(fx.toInt, fy.toInt), Pos(tx.toInt, ty.toInt))

    extension (self: Vector[Line])
      def countPointsWithOverlap: Int =
        self
          .flatMap(_.points)
          .groupBy(identity)
          .count((_, p: Vector[Pos]) => p.length > 1)

  lazy val pt1: Int =
    lines
      .filterNot(_.isDiagonal)
      .countPointsWithOverlap

  // lazy val pt2: Int =
  //  lines.countPointsWithOverlap

  answer(1)(pt1)

  // answer(2)(pt2)
