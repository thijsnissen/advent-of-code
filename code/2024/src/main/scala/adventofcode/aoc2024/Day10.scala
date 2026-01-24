package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Pos
import utilities.Utilities.sumBy

object Day10 extends AdventOfCode(Prod):
  val topographicMap: TopographicMap =
    TopographicMap.unit:
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (i, x) <- l.zipWithIndex
      yield Pos(x, y) -> i.asDigit

  type TopographicMap = Map[Pos, Int]

  object TopographicMap:
    def unit(tm: IterableOnce[(Pos, Int)]): TopographicMap =
      Map.from(tm).withDefaultValue(-1)

    extension (self: TopographicMap)
      def trailheads: Set[Pos] =
        self
          .filter((_, h) => h == 0)
          .keySet

      def hikingTrails(p: Pos): List[Pos] =
        @tailrec def loop(todo: List[Pos], acc: List[Pos]): List[Pos] =
          todo.headOption match
            case None                    => acc
            case Some(p) if self(p) == 9 => loop(todo.tail, p :: acc)
            case Some(p)                 =>
              loop(
                todo.tail :::
                  p
                    .axisOffsetsFn(x => self(x) == self(p) + 1)
                    .toList,
                acc
              )

        loop(List(p), List.empty[Pos])

      def score(p: Pos): Int =
        hikingTrails(p).distinct.length

      def rating(p: Pos): Int =
        hikingTrails(p).length

  import TopographicMap.*

  override lazy val pt1: Int =
    topographicMap
      .trailheads
      .sumBy(topographicMap.score)

  override lazy val pt2: Int =
    topographicMap
      .trailheads
      .sumBy(topographicMap.rating)
