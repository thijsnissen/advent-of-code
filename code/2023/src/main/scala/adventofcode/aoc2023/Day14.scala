package adventofcode
package aoc2023

import scala.collection.immutable.SortedMap
import utilities.AdventOfCode.*
import utilities.Cycle
import utilities.Pos
import utilities.Utilities.sumBy

object Day14 extends AdventOfCode(Prod):
  import Platform.north

  val platform: Platform =
    val positions: Iterator[(Pos, Char)] =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
      yield Pos(x, y) -> c

    Platform.fromMap(positions.toMap)

  type Platform = SortedMap[Pos, Char]

  object Platform:
    def fromMap(map: Map[Pos, Char])(using Ordering[Pos]): Platform =
      SortedMap.empty[Pos, Char] ++ map

    extension (self: Platform)
      def roundedRocks: Platform =
        self.filter((_, r: Char) => r == 'O')

      def tilt(dir: Pos)(using Ordering[Pos]): Platform =
        @tailrec def loop(platform: Platform, p: Pos): Pos =
          platform.get(p + dir) match
            case Some(c) if c == '.' => loop(platform, p + dir)
            case _                   => p

        Platform.fromMap(roundedRocks).foldLeft(self):
          case (acc: Platform, (p: Pos, c: Char)) =>
            val moved: Pos = loop(acc, p)

            if moved != p then acc.updated(moved, c).updated(p, '.') else acc

      def cycle: Platform =
        val directions: Set[(Pos, Ordering[Pos])] =
          Set(
            Pos(0, -1) -> north,
            Pos(-1, 0) -> west,
            Pos(0, 1)  -> south,
            Pos(1, 0)  -> east
          )

        directions.foldLeft(self):
          case (acc: Platform, (dir: Pos, ord: Ordering[Pos])) =>
            acc.tilt(dir)(using ord)

      def totalLoad: Int =
        val (max, _): (Pos, Char) =
          self.maxBy((p: Pos, _) => p.y)

        self
          .roundedRocks
          .sumBy((p: Pos, _) => max.y + 1 - p.y)

    given north: Ordering[Pos] =
      Ordering.fromLessThan: (a, b) =>
        a.y < b.y || (a.y == b.y && a.x < b.x)

    given west: Ordering[Pos] =
      Ordering.fromLessThan: (a, b) =>
        a.x < b.x || (a.x == b.x && a.y < b.y)

    given south: Ordering[Pos] =
      Ordering.fromLessThan: (a, b) =>
        a.y > b.y || (a.y == b.y && a.x < b.x)

    given east: Ordering[Pos] =
      Ordering.fromLessThan: (a, b) =>
        a.x > b.x || (a.x == b.x && a.y < b.y)

  import Platform.*

  lazy val pt1: Int =
    platform.tilt(Pos(0, -1)).totalLoad

  lazy val pt2: Int =
    val cycle: Cycle[Platform] =
      Cycle.find((p: Platform) => p.cycle, platform)(_.totalLoad)

    val iterator: Iterator[Platform] =
      Iterator.iterate(platform)(_.cycle)

    iterator
      .drop(
        cycle.stemLength + (1_000_000_000 - cycle.stemLength) % cycle.cycleLength
      )
      .next
      .totalLoad

  answer(1)(pt1)

  answer(2)(pt2)
