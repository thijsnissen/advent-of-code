package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Pos

object Day23 extends AdventOfCode(Prod):
  val hikingTrails: Map[Pos, Char] =
    val result: Iterator[(Pos, Char)] =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c != '#'
      yield Pos(x, y) -> c

    result.toMap.withDefaultValue('#')

  case class Trails(hikingTrails: Map[Pos, Char]):
    lazy val yMin: Pos = hikingTrails.minBy((p: Pos, _) => p.y)._1

    lazy val yMax: Pos = hikingTrails.maxBy((p: Pos, _) => p.y)._1

    lazy val start: Pos =
      hikingTrails
        .find((p: Pos, c: Char) => p.y == yMin.y && c == '.').get._1

    lazy val target: Pos =
      hikingTrails.find((p: Pos, c: Char) => p.y == yMax.y && c == '.').get._1

    lazy val intersections: Set[Pos] =
      hikingTrails
        .filter((p: Pos, _) => neighbours(p).size >= 3)
        .keySet + start + target

    def neighbours(p: Pos): Set[Pos] = p.axisOffsetsFn(hikingTrails.contains)

    def neighboursWithSlopes(p: Pos): Set[Pos] =
      val next: Set[Pos] = hikingTrails(p) match
        case '<' => Set(p - Pos(1, 0))
        case '>' => Set(p + Pos(1, 0))
        case '^' => Set(p - Pos(0, 1))
        case 'v' => Set(p + Pos(0, 1))
        case _   => p.axisOffsets

      next.filter(hikingTrails.contains)

    def adjacencyList(withSlopes: Boolean): Map[Pos, Set[(Pos, Int)]] =
      @tailrec def loop(
        todo: Vector[(Pos, Int)],
        seen: Set[Pos],
        acc: Set[(Pos, Int)]
      ): Set[(Pos, Int)] =
        todo.headOption match
          case None       => acc
          case Some(p, i) =>
            if intersections.contains(p) && i > 0 then
              loop(todo.tail, seen + p, acc + (p -> i))
            else
              val next =
                if withSlopes then neighboursWithSlopes(p) else neighbours(p)

              loop(
                todo.tail ++ next.diff(seen).map(_ -> (i + 1)),
                seen + p,
                acc
              )

      intersections
        .map: (p: Pos) =>
          p -> loop(Vector(p -> 0), Set.empty[Pos], Set.empty[(Pos, Int)])
        .toMap

    def longestPath(withSlopes: Boolean): Int =
      def loop(pos: Pos, adj: Map[Pos, Set[(Pos, Int)]], acc: Int): Int =
        if pos == target then acc
        else
          adj.get(pos) match
            case None    => 0
            case Some(a) =>
              a.map((p: Pos, i: Int) => loop(p, adj - pos, acc + i)).max

      loop(start, adjacencyList(withSlopes), 0)

  override lazy val pt1: Int =
    Trails(hikingTrails).longestPath(withSlopes = true)

  override lazy val pt2: Int =
    Trails(hikingTrails).longestPath(withSlopes = false)
