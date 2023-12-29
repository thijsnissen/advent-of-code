package adventofcode
package aoc2018

import utilities.AdventOfCode.*
import utilities.Pos3D

object Day23 extends AdventOfCode(Prod):
  val nanobots: Vector[Nanobot] =
    input
      .linesIterator
      .collect:
        case s"pos=<$x,$y,$z>, r=$r" =>
          Nanobot(Pos3D(x.toInt, y.toInt, z.toInt), r.toInt)
      .toVector

  case class Nanobot(pos: Pos3D, signalRadius: Long):
    def nanobotsInRange(nanobots: Vector[Nanobot]): Vector[Nanobot] =
      nanobots.filter(n => n.pos.manhattan(pos) <= signalRadius)

  lazy val pt1: Int =
    nanobots
      .maxBy(_.signalRadius)
      .nanobotsInRange(nanobots)
      .length

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
