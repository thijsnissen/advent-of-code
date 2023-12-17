package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Dijkstra.*
import utilities.Grid
import utilities.WeightedGraph

object Day17 extends AdventOfCode(Prod):
  import Direction.*

  val trafficPatterns: TrafficPatterns =
    Grid.unit:
      input
        .linesIterator
        .map(_.map(_.asDigit).toVector)
        .toVector

  enum Direction:
    case Horizontal, Vertical, Unknown

  type TrafficPatterns = Grid[Int]

  object TrafficPatterns:
    extension (self: WeightedGraph[(Int, Int, Direction)])
      def leastHeatlossTo(tX: Int, tY: Int): Int =
        val (distances: Map[(Int, Int, Direction), Int], _) =
          self.shortestPathTree((0, 0, Unknown))

        val (_, weight) =
          distances
            .filter:
              case ((x, y, _), _) => x == tX - 1 && y == tY - 1
            .minBy:
              case ((_, _, _), weight) => weight

        weight

    extension (self: TrafficPatterns)
      def cost(sX: Int, sY: Int, tX: Int, tY: Int): Int =
        val costs: IndexedSeq[Int] =
          for
            y <- (sY min tY) to (sY max tY)
            x <- (sX min tX) to (sX max tX)
          yield self(x)(y)

        costs.sum - self(sX)(sY)

      def toGraph(
        minOffset: Int,
        maxOffset: Int
      ): WeightedGraph[(Int, Int, Direction)] =
        val (xMax: Int, yMax: Int) = self.size

        WeightedGraph.unit: (sX: Int, sY: Int, sDir: Direction) =>
          lazy val horizontal: IndexedSeq[(Int, Int, Direction)] =
            (minOffset to maxOffset).flatMap: (i: Int) =>
              Seq((sX + i, sY, Horizontal), (sX - i, sY, Horizontal))

          lazy val vertical: IndexedSeq[(Int, Int, Direction)] =
            (minOffset to maxOffset).flatMap: (i: Int) =>
              Seq((sX, sY + i, Vertical), (sX, sY - i, Vertical))

          val next: IndexedSeq[(Int, Int, Direction)] = sDir match
            case Horizontal => vertical
            case Vertical   => horizontal
            case Unknown    => horizontal ++ vertical

          next
            .filter: (x: Int, y: Int, _) =>
              x >= 0 && x <= xMax - 1 && y >= 0 && y <= yMax - 1
            .foldLeft(Map.empty[(Int, Int, Direction), Int]):
              case (acc, (tX: Int, tY: Int, tD: Direction)) =>
                acc + ((tX, tY, tD) -> cost(sX, sY, tX, tY))

    given Ordering[(Int, Int, Direction)] =
      Ordering.by((x: Int, y: Int, _) => (x, y))

  import TrafficPatterns.*

  lazy val pt1: Int =
    val (xMax: Int, yMax: Int) = trafficPatterns.size

    trafficPatterns
      .toGraph(1, 3)
      .leastHeatlossTo(xMax, yMax)

  lazy val pt2: Int =
    val (xMax: Int, yMax: Int) = trafficPatterns.size

    trafficPatterns
      .toGraph(4, 10)
      .leastHeatlossTo(xMax, yMax)

  answer(1)(pt1)

  answer(2)(pt2)
