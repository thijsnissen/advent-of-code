package adventofcode
package aoc2023

import scala.collection.immutable.SortedSet
import utilities.AdventOfCode.*
import utilities.Graph
import utilities.GraphTraversal.*
import utilities.Orderings.posReadingOrder
import utilities.Pos

object Day10 extends AdventOfCode(Test):
  val surface: Surface =
    val sketch =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c != '.'
      yield Pos(x, y) -> c

    sketch.toMap.withDefaultValue('.')

  type Surface = Map[Pos, Char]

  object Surface:
    extension (self: Surface)
      def offsets(pos: Pos): Set[Pos] =
        val adj: Set[Pos] = self(pos) match
          case '-' => Set(pos + Pos(1, 0), pos - Pos(1, 0))
          case '|' => Set(pos - Pos(0, 1), pos + Pos(0, 1))
          case 'L' => Set(pos + Pos(1, 0), pos - Pos(0, 1))
          case 'J' => Set(pos - Pos(1, 0), pos - Pos(0, 1))
          case '7' => Set(pos - Pos(1, 0), pos + Pos(0, 1))
          case 'F' => Set(pos + Pos(1, 0), pos + Pos(0, 1))

        adj.filter((p: Pos) => surface(p) != '.')

      def startEnd: (Pos, Pos) =
        val start: Pos =
          self
            .find((_, c: Char) => c == 'S')
            .map((p: Pos, _) => p)
            .get

        val north: Char = self(start - Pos(0, 1))
        val east: Char  = self(start + Pos(1, 0))
        val south: Char = self(start + Pos(0, 1))
        val west: Char  = self(start - Pos(1, 0))

        val pos: List[Pos] = List(
          Option.when(north == '|' || north == '7' || north == 'F'):
            start - Pos(0, 1)
          ,
          Option.when(east == '-' || east == 'J' || east == '7'):
            start + Pos(1, 0)
          ,
          Option.when(south == '|' || south == 'L' || south == 'J'):
            start + Pos(0, 1)
          ,
          Option.when(west == '-' || west == 'L' || west == 'F'):
            start - Pos(1, 0)
        ).flatten

        assert(pos.sizeIs == 2)

        (pos(0), pos(1))

      def findLoop: List[Pos] =
        val (start: Pos, end: Pos) = surface.startEnd

        val graph: Graph[Pos] =
          Graph.unit: (p: Pos) =>
            SortedSet.empty[Pos] ++
              surface.offsets(p).filter((p: Pos) => surface(p) != 'S')

        graph.breadthFirstSearchPathTo(start)(_ == end) match
          case Some(path) =>
            self.offsets(start).intersect(self.offsets(end)).head :: path
          case None => sys.error(s"No path found from $start to $end.")

  import Surface.*

  lazy val pt1: Int =
    (surface.findLoop.length + 1) / 2

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
