package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Pos
import utilities.Pos.*
import utilities.Utilities.picksTheorem

object Day18 extends AdventOfCode(Prod):
  val digPlan: Vector[Plan] =
    input
      .linesIterator
      .collect:
        case s"$vector $dist (#$color)" =>
          Plan(Plan.vectorFromString(vector), dist.toInt, color)
      .toVector

  case class Plan(vector: Pos, dist: Int, color: String):
    def dig(from: Pos): Pos =
      from + vector * dist

    def extract: Plan =
      copy(
        vector = Plan.vectorFromString(color.takeRight(1)),
        dist = Integer.parseInt(color.init, 16)
      )

  object Plan:
    def vectorFromString(s: String): Pos =
      s match
        case "U" | "3" => Pos(0, -1)
        case "D" | "1" => Pos(0, 1)
        case "L" | "2" => Pos(-1, 0)
        case "R" | "0" => Pos(1, 0)

    extension (self: Vector[Plan])
      def cubicMeters: Long =
        val (edges: Vector[Pos], boundaryPoints: Long) =
          self.foldLeft((Vector(Pos.zero), 0L)):
            case ((acc: Vector[Pos], counter: Long), plan) =>
              (plan.dig(acc.head) +: acc, counter + plan.dist)

        val interiorPoints: Long =
          picksTheorem(edges.shoelaceFormula, boundaryPoints)

        interiorPoints + boundaryPoints

  import Plan.*

  override lazy val pt1: Long =
    digPlan.cubicMeters

  override lazy val pt2: Long =
    digPlan.map(_.extract).cubicMeters
