package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Cycle
import utilities.Pos
import utilities.Utilities.sumBy

// TODO: This needs fixing...

object Day14 extends AdventOfCode(Test):
  val platform: Platform =
    val positions: Iterator[(Pos, Char)] =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
      yield Pos(x, y) -> c

    positions.toVector

  type Platform = Vector[(Pos, Char)]

  object Platform:
    extension (self: Platform)
      def replace(k: Pos, v: Char): Platform =
        val index = self.indexWhere:
          case (p: Pos, _) => p == k

        self.updated(index, (k, v))

      def roundedRocks: Platform =
        self.filter((_, r: Char) => r == 'O')

      def tilt(dir: Pos)(using Ordering[Pos]): Platform =
        @tailrec def move(platform: Platform, pos: Pos): Pos =
          platform.find((p: Pos, _: Char) => p == pos + dir) match
            case Some(_, '.') => move(platform, pos + dir)
            case _            => pos

        roundedRocks.sortBy((p: Pos, _) => p).foldLeft(self):
          case (acc: Platform, (p: Pos, _)) =>
            val moved: Pos = move(acc, p)

            if moved != p then acc.replace(moved, 'O').replace(p, '.') else acc

      def cycle: Platform =
        val directions: Set[(Pos, Ordering[Pos])] =
          Set(
            Pos(0, -1) -> north,
            Pos(-1, 0) -> west,
            Pos(0, 1)  -> south,
            Pos(1, 0)  -> east
          )

        directions.foldLeft(self):
          (acc: Platform, dir: (Pos, Ordering[Pos])) =>
            val (p: Pos, o: Ordering[Pos]) = dir

            acc.tilt(p)(using o)

      def totalLoad: Int =
        val (max, _): (Pos, Char) =
          self.maxBy((p: Pos, _) => p.y)

        self
          .roundedRocks
          .sumBy((p: Pos, _) => max.y + 1 - p.y)

    given north: Ordering[Pos] = Ordering.by((p: Pos) => p.y)

    given west: Ordering[Pos] = Ordering.by((p: Pos) => p.x)

    given south: Ordering[Pos] = Ordering.by((p: Pos) => -p.y)

    given east: Ordering[Pos] = Ordering.by((p: Pos) => -p.x)

  import Platform.*

  lazy val pt1: Int =
    platform.tilt(Pos(0, -1))(using Platform.north).totalLoad

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
