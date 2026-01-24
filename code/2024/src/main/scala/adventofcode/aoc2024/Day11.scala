package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Cache
import utilities.Utilities.sumBy

object Day11 extends AdventOfCode(Prod):
  val stones: Vector[Stone] =
    input
      .split(" ")
      .map(_.trim.toLong)
      .toVector

  type Stone = Long

  object Stone:
    extension (self: Stone)
      def blink(times: Int): Long =
        lazy val cache: (Stone, Int) => Long =
          Cache.memoize:
            case (_, 0) => 1
            case (0, b) => cache(1, b - 1)
            case (s, b) if s.toString.length % 2 == 0 =>
              val (l, r) = s.toString.splitAt(s.toString.length / 2)

              cache(l.toInt, b - 1) + cache(r.toInt, b - 1)
            case (s, b) => cache(s * 2024, b - 1)

        cache(self, times)

  import Stone.*

  override lazy val pt1: Long =
    stones.sumBy(_.blink(times = 25))

  override lazy val pt2: Long =
    stones.sumBy(_.blink(times = 75))
