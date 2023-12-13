package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day13 extends AdventOfCode(Prod):
  val patterns: Vector[Pattern] =
    input
      .split("\n\n")
      .map(Pattern.fromInput)
      .toVector

  type Pattern = Vector[String]

  object Pattern:
    def fromInput(s: String): Pattern =
      s.linesIterator.toVector

    def reflectionLine(pattern: Pattern): Option[Int] =
      (1 until pattern.length)
        .find: (i: Int) =>
          val (l: Pattern, r: Pattern) =
            pattern.splitAt(i)

          val size: Int = l.length min r.length

          l.reverse.take(size) == r.take(size)

    def smudgedReflectionLine(pattern: Pattern): Option[Int] =
      (1 until pattern.length)
        .find: (i: Int) =>
          val (l: Pattern, r: Pattern) =
            pattern.splitAt(i)

          val size: Int = l.length min r.length

          l.reverse.take(size)
            .zip(r.take(size))
            .map((l: String, r: String) => l.zip(r).count(_ != _))
            .sum == 1

    def summarize(p: Pattern)(fn: Pattern => Option[Int]): Int =
      fn(p.transpose.map(_.mkString)).getOrElse(0) + 100 * fn(p).getOrElse(0)

  import Pattern.*

  lazy val pt1: Int =
    patterns.foldLeft(0): (acc: Int, p: Pattern) =>
      acc + summarize(p)(reflectionLine)

  lazy val pt2: Int =
    patterns.foldLeft(0): (acc: Int, p: Pattern) =>
      acc + summarize(p)(smudgedReflectionLine)

  answer(1)(pt1)

  answer(2)(pt2)
