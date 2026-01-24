package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Box
import utilities.Pos
import utilities.Utilities.exponentialBinarySearch

object Day18 extends AdventOfCode(Prod):
  val bytes: Bytes =
    Bytes.fromString(input)

  type Bytes = Vector[Pos]

  object Bytes:
    def fromString(s: String): Bytes =
      s
        .linesIterator
        .map:
          case s"$x,$y" => Pos(x.toInt, y.toInt)
        .toVector

    extension (self: Bytes)
      def toExit(start: Pos, exit: Pos, corrupted: Int): Option[(Pos, Int)] =
        val corruptedBytes = self.take(corrupted)
        val bound          = Box(start, exit)

        @tailrec def loop(
          todo: Vector[(Pos, Int)],
          v: Set[Pos]
        ): Option[(Pos, Int)] =
          todo.headOption match
            case None                        => None
            case Some(p, d) if p == exit     => Some(p, d)
            case Some(p, _) if v.contains(p) => loop(todo.tail, v)
            case Some(p, d)                  =>
              val next =
                p
                  .axisOffsetsFn: p =>
                    !corruptedBytes.contains(p) && bound.contains(p)
                  .map(p => (p, d + 1))

              loop(todo.tail ++ next, v + p)

        loop(Vector((start, 0)), Set.empty[Pos])

      def blocking(start: Pos, exit: Pos): Pos =
        val i =
          exponentialBinarySearch(0, Some(bytes.length)): i =>
            toExit(
              start = start,
              exit = exit,
              corrupted = i
            ).isDefined

        self(i - 1)

  import Bytes.*

  override lazy val pt1: Int =
    val (_, distance) =
      bytes
        .toExit(
          start = Pos.zero,
          exit = if getEnv == Test then Pos(6, 6) else Pos(70, 70),
          corrupted = if getEnv == Test then 12 else 1024
        )
        .get

    distance

  override lazy val pt2: String =
    val Pos(x, y) =
      bytes.blocking(
        start = Pos.zero,
        exit = if getEnv == Test then Pos(6, 6) else Pos(70, 70)
      )

    s"$x,$y"
