package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val myInput: Vector[Long] =
    input
      .linesIterator
      .map(_.toLong)
      .toVector

  def firstSumSeenTwice(input: Vector[Long]): Long =
    @annotation.tailrec
    def go(in: Vector[Long], seen: Set[Long] = Set(0), acc: Long = 0): Long =
      if in.isEmpty then go(input, seen, acc)
      else if seen.contains(acc + in.head) then acc + in.head
      else go(in.tail, seen + (acc + in.head), acc + in.head)

    go(input)

  override lazy val pt1: Long =
    myInput.sum

  override lazy val pt2: Long =
    firstSumSeenTwice(myInput)
