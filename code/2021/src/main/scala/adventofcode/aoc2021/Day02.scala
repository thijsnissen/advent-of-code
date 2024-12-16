package adventofcode
package aoc2021

import utilities.AdventOfCode.*
import utilities.Pos

object Day02 extends AdventOfCode(Prod):
  val course: Vector[Pos] =
    input
      .linesIterator
      .collect:
        case s"forward $x" => Pos(x.toInt, 0)
        case s"up $x"      => Pos(0, -x.toInt)
        case s"down $x"    => Pos(0, x.toInt)
      .toVector

  lazy val pt1: Int =
    val Pos(hpos, depth) =
      course.foldLeft(Pos.zero): (acc, pos) =>
        acc + pos

    hpos * depth

  lazy val pt2: Int =
    val (Pos(hpos, _), depth) =
      course.foldLeft((Pos.zero, 0)):
        case ((pos, depth), Pos(0, aim)) =>
          (pos.copy(y = pos.y + aim), depth)
        case ((pos, depth), Pos(hpos, 0)) =>
          (pos.copy(x = pos.x + hpos), depth + pos.y * hpos)
        case (acc, _) => acc

    hpos * depth

  answer(1)(pt1)

  answer(2)(pt2)
