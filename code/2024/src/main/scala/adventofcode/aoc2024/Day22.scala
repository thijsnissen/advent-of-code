package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Utilities.sumBy

object Day22 extends AdventOfCode(Prod):
  val buyers: Vector[Long] =
    input
      .linesIterator
      .map(_.toLong)
      .toVector

  extension (self: Long)
    def next: Long =
      val step1 = (i: Long) => i.mix(i * 64).prune
      val step2 = (i: Long) => i.mix(i / 32).prune
      val step3 = (i: Long) => i.mix(i * 2048).prune

      step1 andThen step2 andThen step3 apply self

    def mix(s: Long): Long =
      self ^ s

    def prune: Long =
      self % 16777216

    def ones: Long =
      self % 10

    def changes: Iterator[(Seq[Long], Long)] =
      Iterator
        .iterate(self)(_.next)
        .sliding(5)
        .map: ss =>
          ss
            .zip(ss.tail)
            .map((a, b) => b.ones - a.ones) -> ss.last.ones
        .take(2000)
        .distinctBy((c, _) => c)

  lazy val pt1: Long =
    buyers.sumBy: b =>
      Iterator
        .iterate(b)(_.next)
        .drop(2000)
        .next

  lazy val pt2: Long =
    buyers
      .foldLeft(Map.empty[Seq[Long], Long].withDefaultValue(0L)): (acc, b) =>
        b.changes.foldLeft(acc):
          case (acc, (c, p)) => acc.updated(c, acc(c) + p)
      .values
      .max

  answer(1)(pt1)

  answer(2)(pt2)
