package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Grid
import utilities.Grid.*

object Day08 extends AdventOfCode(Prod):
  val antennas: Grid[Antenna] =
    Grid.unit:
      input
        .linesIterator
        .map(_.toVector)
        .toVector

  type Antenna = Char
  type Pos     = (Int, Int)

  object Antenna:
    extension (self: Grid[Antenna])
      def combinations: Set[(Pos, Pos)] =
        self
          .iterateWithIndex
          .filterNot:
            case (_, _, c) => c == '.'
          .toVector
          .groupMap((_, _, c) => c)((x, y, _) => (x, y))
          .values
          .flatMap(_.combinations(2))
          .collect:
            case Vector(a: Pos, b: Pos) => (a, b)
          .toSet

      def contains(p: Pos): Boolean =
        val (xMax, yMax) = self.size
        val (x, y)       = p

        x >= 0 && x <= xMax - 1 && y >= 0 && y <= yMax - 1

      def antinodes(a: Pos, b: Pos)(
        withResonantHarmonics: Boolean
      ): Vector[Pos] =
        val ((ax, ay), (bx, by)) = (a, b)
        val (dx, dy)             = ((ax - bx).abs, (ay - by).abs)

        @tailrec def loop(acc: Vector[Pos], i: Int = 1): Vector[Pos] =
          val ans = Vector(
            (
              if ax < bx then ax - dx * i else ax + dx * i,
              if ay < by then ay - dy * i else ay + dy * i
            ),
            (
              if bx < ax then bx - dx * i else bx + dx * i,
              if by < ay then by - dy * i else by + dy * i
            )
          )

          if ans.exists(contains) && withResonantHarmonics then
            loop(acc ++ ans, i + 1)
          else
            acc ++ ans

        loop(if withResonantHarmonics then Vector(a, b) else Vector.empty[Pos])

  import Antenna.*

  lazy val pt1: Int =
    antennas
      .combinations
      .flatMap(antennas.antinodes(_, _)(withResonantHarmonics = false))
      .count(antennas.contains)

  lazy val pt2: Int =
    antennas
      .combinations
      .flatMap(antennas.antinodes(_, _)(withResonantHarmonics = true))
      .count(antennas.contains)

  answer(1)(pt1)

  answer(2)(pt2)
