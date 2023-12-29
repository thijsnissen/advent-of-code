package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Box
import utilities.Pos

object Day21 extends AdventOfCode(Prod):
  val garden: Garden =
    val map: Iterator[(Pos, Char)] =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c != '#'
      yield Pos(x, y) -> c

    map.toMap.withDefaultValue('.')

  type Garden = Map[Pos, Char]

  object Garden:
    given Ordering[(Pos, Int)] = Ordering.by((_, i: Int) => -i)

    extension (self: Garden)
      def steps(start: Pos, target: Int): Map[Pos, Int] =
        import scala.collection.mutable

        val Box(min, max): Box = Box.bounding(self.keys)

        @tailrec def loop(
          todo: mutable.PriorityQueue[(Pos, Int)],
          visited: Map[Pos, Int]
        ): Map[Pos, Int] =
          if todo.isEmpty then visited
          else
            val (p: Pos, s: Int) = todo.dequeue

            val next: Set[(Pos, Int)] =
              if s <= target && !visited.contains(p) then
                p.axisOffsetsFn(p => self.contains(Pos(p.x, p.y))).map(
                  _ -> (s + 1)
                )
              else Set.empty[(Pos, Int)]

            loop(todo ++ next, visited + (p -> s))

        loop(mutable.PriorityQueue(start -> 0), Map.empty[Pos, Int])

      def inifiniteSteps(start: Pos, target: Int): Map[Pos, Int] =
        ???

  import Garden.*

  lazy val pt1: Int =
    val (start: Pos, _) = garden.find((_, c) => c == 'S').get
    val target: Int     = if getEnv == Test then 6 else 64

    garden
      .steps(start, target)
      .count((_, s: Int) => s % 2 == 0)

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
