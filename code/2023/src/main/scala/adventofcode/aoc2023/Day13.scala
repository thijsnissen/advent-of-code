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

    def reflectionLine(pattern: Pattern)(reflection: Int => Boolean)
      : Option[Int] = (1 until pattern.length)
      .find: (i: Int) =>
        val (l: Pattern, r: Pattern) =
          pattern.splitAt(i)

        val size: Int = l.length min r.length

        reflection:
          l.reverse.take(size)
            .zip(r.take(size))
            .map((l: String, r: String) => l.zip(r).count(_ != _))
            .sum

    extension (self: Pattern)
      def summarize(reflection: Int => Boolean): Int =
        reflectionLine(self.transpose.map(_.mkString))(reflection).getOrElse(
          0
        ) +
          100 * reflectionLine(self)(reflection).getOrElse(0)

  import Pattern.*

  override lazy val pt1: Int =
    patterns.foldLeft(0): (acc: Int, p: Pattern) =>
      acc + p.summarize(_ == 0)

  override lazy val pt2: Int =
    patterns.foldLeft(0): (acc: Int, p: Pattern) =>
      acc + p.summarize(_ == 1)
