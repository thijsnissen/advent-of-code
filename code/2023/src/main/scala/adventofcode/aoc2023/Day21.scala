package adventofcode
package aoc2023

import adventofcode.utilities.Utilities.+%
import utilities.AdventOfCode.*
import utilities.Box
import utilities.Pos

// TODO: figure out a general solution:
// - https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/keb7zhu/
// - https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/keaj1mg/
object Day21 extends AdventOfCode(Prod):
  val garden: Garden =
    val map: Iterator[(Pos, Char)] =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c != '#'
      yield Pos(x, y) -> c

    Garden(map.toMap)

  case class Garden(garden: Map[Pos, Char]):
    lazy val (start: Pos, _) = garden.find((_, c) => c == 'S').get

    lazy val (min: Pos, max: Pos) =
      val box: Box = Box.bounding(garden.keys)

      (box.min - Pos(1, 1), box.max + Pos(1, 1))

    def inInfinite(p: Pos): Boolean =
      garden.contains(Pos(p.x +% max.x, p.y +% max.y))

    def steps(target: Int): Map[Pos, Int] =
      @tailrec def loop(
        todo: Vector[(Pos, Int)],
        visited: Map[Pos, Int]
      ): Map[Pos, Int] =
        todo.headOption match
          case None => visited
          case Some(p: Pos, s: Int) =>
            val next: Set[(Pos, Int)] =
              if s < target && !visited.contains(p) then
                p.axisOffsetsFn(inInfinite).map(_ -> (s + 1))
              else Set.empty[(Pos, Int)]

            loop(todo.tail ++ next, visited + (p -> s))

      loop(Vector(start -> 0), Map.empty[Pos, Int])

  lazy val pt1: Int =
    val target: Int = if getEnv == Test then 6 else 64

    garden
      .steps(target)
      .count((_, s: Int) => s % 2 == target % 2)

  lazy val pt2: Long =
    if getEnv == Test then garden.steps(100).count((_, s: Int) => s % 2 == 0)
    else
      assert(garden.max.x == garden.max.y)
      assert(garden.start == Pos(garden.max.x / 2, garden.max.y / 2))

      assert:
        (garden.min.x to garden.max.x)
          .forall(x => garden.inInfinite(Pos(x, garden.start.y)))

      assert:
        (garden.min.y to garden.max.y)
          .forall(y => garden.inInfinite(Pos(garden.start.x, y)))

      // this function I graciously borrowed from @nmcb
      def quadratic(y0: Long, y1: Long, y2: Long)(x: Long): Long =
        y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

      val target: Int    = 26501365
      val width: Int     = garden.max.x
      val halfWidth: Int = width / 2

      val y0: Int =
        garden
          .steps(halfWidth)
          .count((_, s: Int) => s % 2 == halfWidth % 2)

      val y1: Int =
        garden
          .steps(width + halfWidth)
          .count((_, s: Int) => s % 2 == (width + halfWidth) % 2)

      val y2: Int =
        garden
          .steps(2 * width + halfWidth)
          .count((_, s: Int) => s % 2 == (2 * width + halfWidth) % 2)

      val x: Int = (target - halfWidth) / width

      quadratic(y0, y1, y2)(x)

  answer(1)(pt1)

  answer(2)(pt2)
