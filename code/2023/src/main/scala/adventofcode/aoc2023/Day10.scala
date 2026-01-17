package adventofcode
package aoc2023

import scala.collection.immutable.SortedSet
import utilities.AdventOfCode.*
import utilities.Box
import utilities.Graph
import utilities.GraphTraversal.*
import utilities.Orderings.posReadingOrder
import utilities.Pos

object Day10 extends AdventOfCode(Prod):
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
      def startPos: Pos =
        self
          .find((_, c: Char) => c == 'S')
          .map((p: Pos, _) => p)
          .get

      def startOffset: (Pos, Pos) =
        val start: Pos = startPos

        val north: Option[(Pos, Char)] =
          self.find((p: Pos, _) => p == start - Pos(0, 1))
        val east: Option[(Pos, Char)] =
          self.find((p: Pos, _) => p == start + Pos(1, 0))
        val south: Option[(Pos, Char)] =
          self.find((p: Pos, _) => p == start + Pos(0, 1))
        val west: Option[(Pos, Char)] =
          self.find((p: Pos, _) => p == start - Pos(1, 0))

        val pipes: List[(Pos, Char)] = List(
          north.filter((_, c: Char) => c == '|' || c == '7' || c == 'F'),
          east.filter((_, c: Char) => c == '-' || c == 'J' || c == '7'),
          south.filter((_, c: Char) => c == '|' || c == 'L' || c == 'J'),
          west.filter((_, c: Char) => c == '-' || c == 'L' || c == 'F')
        ).flatten

        assert(pipes.sizeIs == 2)

        val (sPos, _) = pipes(0)
        val (ePos, _) = pipes(1)

        (sPos, ePos)

      def offsets(pos: Pos): Set[Pos] =
        val adj: Set[Pos] = self(pos) match
          case '-' => Set(pos + Pos(1, 0), pos - Pos(1, 0))
          case '|' => Set(pos - Pos(0, 1), pos + Pos(0, 1))
          case 'L' => Set(pos + Pos(1, 0), pos - Pos(0, 1))
          case 'J' => Set(pos - Pos(1, 0), pos - Pos(0, 1))
          case '7' => Set(pos - Pos(1, 0), pos + Pos(0, 1))
          case 'F' => Set(pos + Pos(1, 0), pos + Pos(0, 1))

        adj.filterNot((p: Pos) => self(p) == '.')

      def loop: List[Pos] =
        val (start: Pos, end: Pos) = self.startOffset

        val graph: Graph[Pos] =
          Graph.unit: (p: Pos) =>
            SortedSet.empty[Pos] ++
              self.offsets(p).filter((p: Pos) => self(p) != 'S')

        graph.breadthFirstSearchPathTo(start)(_ == end) match
          case Some(path) => startPos :: path
          case None       => sys.error(s"No path found from $start to $end.")

      def inside(pipes: List[Pos]): Set[Pos] =
        val toCount: Set[Char] = Set('|', 'F', '7', 'S') // S == 7

        @tailrec def loop(
          y: Int,
          x: Iterator[Int],
          acc: Set[Pos],
          inside: Set[Pos],
          count: Boolean
        ): Set[Pos] =
          if !x.hasNext then inside
          else
            val pos: Pos   = Pos(x.next, y)
            val pipe: Char = self(pos)

            // Start counting...
            if pipes.contains(pos) && toCount.contains(pipe) && !count then
              loop(y, x, acc, inside, !count)
            // Stop counting...
            else if pipes.contains(pos) && toCount.contains(pipe) && count then
              loop(y, x, Set.empty[Pos], inside ++ acc, !count)
            // Do count...
            else if !pipes.contains(pos) && count then
              loop(y, x, acc + pos, inside, count)
            // Don't count...
            else
              loop(y, x, acc, inside, count)

        val box: Box = Box.bounding(self.keys)

        val xRange: Range = box.min.x - 1 to box.max.x + 1
        val yRange: Range = box.min.y - 1 to box.max.y + 1

        yRange
          .flatMap:
            loop(_, xRange.iterator, Set.empty[Pos], Set.empty[Pos], false)
          .toSet

  import Surface.*

  lazy val pt1: Int = (surface.loop.length + 1) / 2

  lazy val pt2: Int =
    surface
      .inside(surface.loop)
      .size

  answer(1)(pt1)

  answer(2)(pt2)
