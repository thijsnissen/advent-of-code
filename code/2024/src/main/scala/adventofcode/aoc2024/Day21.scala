package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Cache
import utilities.Pos
import utilities.Utilities.sumBy

object Day21 extends AdventOfCode(Prod):
  val codes: Vector[String] =
    input.linesIterator.toVector

  type Keypad = Map[Pos, Char]

  val numericKeypad: Keypad =
    Map(
      Pos(0, 0) -> '7',
      Pos(1, 0) -> '8',
      Pos(2, 0) -> '9',
      Pos(0, 1) -> '4',
      Pos(1, 1) -> '5',
      Pos(2, 1) -> '6',
      Pos(0, 2) -> '1',
      Pos(1, 2) -> '2',
      Pos(2, 2) -> '3',
      Pos(1, 3) -> '0',
      Pos(2, 3) -> 'A'
    )

  val directionalKeypad: Keypad =
    Map(
      Pos(1, 0) -> '^',
      Pos(2, 0) -> 'A',
      Pos(0, 1) -> '<',
      Pos(1, 1) -> 'v',
      Pos(2, 1) -> '>'
    )

  object Keypad:
    def paths(s: String, keypad: Keypad): Vector[String] =
      s"A$s"
        .sliding(2)
        .foldLeft(Vector("")):
          case (acc, w) =>
            val (s, _) = keypad.find((_, c) => c == w.head).get
            val (e, _) = keypad.find((_, c) => c == w.last).get

            keypad.shortestPaths(s, e).flatMap(p => acc.map(_ + p))

    def complexity(p: String, c: String): Long =
      p.length * c.init.toLong

    def complexity(l: Long, c: String): Long =
      l * c.init.toLong

    extension (self: Keypad)
      def next(p: Pos): Set[(Pos, Char)] =
        Set(
          Pos(1, 0)  -> '>',
          Pos(-1, 0) -> '<',
          Pos(0, 1)  -> 'v',
          Pos(0, -1) -> '^'
        )
          .map((np, c) => p + np -> c)
          .filter((np, _) => self.contains(np))

      def shortestPaths(s: Pos, e: Pos): Vector[String] =
        @tailrec def loop(
          todo: Vector[(Pos, String)],
          acc: Vector[String],
          max: Int = Int.MaxValue
        ): Vector[String] =
          todo.headOption match
            case None                           => acc
            case Some(_, cs) if cs.length > max => acc
            case Some(cp, cs) if cp == e =>
              loop(todo.tail, acc :+ cs + 'A', cs.length + 1)
            case Some(cp, cs) =>
              loop(
                todo.tail ++ next(cp).map((np, nc) => np -> (cs + nc)),
                acc,
                max
              )

        loop(Vector(s -> ""), Vector.empty[String])

    extension (codes: Vector[String])
      def find(robots: Int): Vector[Long] =
        lazy val cache: (Pos, Pos, Int) => Long =
          Cache.memoize: (s, e, r) =>
            if r == 0 then
              directionalKeypad.shortestPaths(s, e).map(_.length).min
            else if r == robots then
              numericKeypad.shortestPaths(s, e).map(solution(_, r - 1)).min
            else
              directionalKeypad.shortestPaths(s, e).map(solution(_, r - 1)).min

        def solution(c: String, r: Int): Long =
          s"A$c"
            .sliding(2)
            .foldLeft(0L):
              case (acc, w) =>
                val keypad =
                  if r == robots then numericKeypad else directionalKeypad

                val (s, _) = keypad.find((_, c) => c == w.head).get
                val (e, _) = keypad.find((_, c) => c == w.last).get

                acc + cache(s, e, r)

        codes.map(solution(_, robots))

  import Keypad.*

  lazy val pt1: Long =
    codes
      .map: c =>
        val r1 = paths(c, numericKeypad)
        val r2 = r1.flatMap(paths(_, directionalKeypad))
        val r3 = r2.flatMap(paths(_, directionalKeypad))

        r3.minBy(_.length)
      .zip(codes)
      .sumBy(complexity(_, _))

  lazy val pt2: Long =
    codes
      .find(robots = 25)
      .zip(codes)
      .sumBy(complexity(_, _))

  answer(1)(pt1)

  answer(2)(pt2)
