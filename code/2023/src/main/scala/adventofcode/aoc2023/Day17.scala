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
    case Up, Right, Down, Left, Unknown

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
        offsetFrom: Int,
        offsetTo: Int
      ): WeightedGraph[(Int, Int, Direction)] =
        val (xMax: Int, yMax: Int) = self.size

        WeightedGraph.unit: (sX: Int, sY: Int, sDir: Direction) =>
          lazy val left: IndexedSeq[(Int, Int, Direction)] =
            (offsetFrom to offsetTo).map((i: Int) => (sX - i, sY, Left))

          lazy val right: IndexedSeq[(Int, Int, Direction)] =
            (offsetFrom to offsetTo).map((i: Int) => (sX + i, sY, Right))

          lazy val up: IndexedSeq[(Int, Int, Direction)] =
            (offsetFrom to offsetTo).map((i: Int) => (sX, sY - i, Up))

          lazy val down: IndexedSeq[(Int, Int, Direction)] =
            (offsetFrom to offsetTo).map((i: Int) => (sX, sY + i, Down))

          val next: IndexedSeq[(Int, Int, Direction)] = sDir match
            case Up | Down    => left ++ right
            case Left | Right => up ++ down
            case Unknown      => left ++ right ++ up ++ down

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

    val graph: WeightedGraph[(Int, Int, Direction)] =
      trafficPatterns.toGraph(1, 3)

    graph.leastHeatlossTo(xMax, yMax)

  lazy val pt2: Int =
    val (xMax: Int, yMax: Int) = trafficPatterns.size

    val graph: WeightedGraph[(Int, Int, Direction)] =
      trafficPatterns.toGraph(4, 10)

    graph.leastHeatlossTo(xMax, yMax)

  answer(1)(pt1)

  answer(2)(pt2)
