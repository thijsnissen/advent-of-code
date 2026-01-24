package adventofcode
package aoc2024

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val (left: List[Int], right: List[Int]) =
    input
      .linesIterator
      .foldLeft((List.empty[Int], List.empty[Int])):
        case ((ls, rs), s"$l $r") => (l.trim.toInt :: ls, r.trim.toInt :: rs)
        case (acc, _)             => acc

  override lazy val pt1: Int =
    left
      .sorted
      .zip(right.sorted)
      .map((l, r) => (l - r).abs)
      .sum

  override lazy val pt2: Int =
    left
      .foldLeft(0): (acc, l) =>
        acc + l * right.count(_ == l)
