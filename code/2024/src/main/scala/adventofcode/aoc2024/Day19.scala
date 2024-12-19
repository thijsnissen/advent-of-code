package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Cache
import utilities.Utilities.sumBy

object Day19 extends AdventOfCode(Prod):
  val (towels: Vector[Towel], designs: Vector[Towel]) =
    val Array(t, d) = input.split("\n\n")

    (
      t.split(",").map(Towel.fromString).toVector,
      d.linesIterator.map(Towel.fromString).toVector
    )

  type Towel = String

  object Towel:
    def fromString(s: String): Towel =
      s.trim

    extension (self: Vector[Towel])
      def strip(d: Towel): Vector[Towel] =
        self.foldLeft(Vector.empty[Towel]): (acc, t) =>
          if d.endsWith(t) then acc :+ d.stripSuffix(t) else acc

      def make(d: Towel): Boolean =
        @tailrec def loop(todo: Vector[Towel]): Boolean =
          todo.headOption match
            case None                 => false
            case Some(d) if d.isEmpty => true
            case Some(d)              => loop(self.strip(d) ++ todo.tail)

        loop(Vector(d))

      def design(d: Towel): Long =
        lazy val cache: Towel => Long =
          Cache.memoize: d =>
            if d.isEmpty then 1
            else self.strip(d).sumBy(cache)

        cache(d)

  import Towel.*

  lazy val pt1: Int =
    designs.count(towels.make)

  lazy val pt2: Long =
    designs.sumBy(towels.design)

  answer(1)(pt1)

  answer(2)(pt2)
