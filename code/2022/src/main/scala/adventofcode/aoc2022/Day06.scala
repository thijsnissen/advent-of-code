package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day06 extends AdventOfCode(Prod):
  @annotation.tailrec
  def findMarkerLocation(
    dataStreamBuffer: String,
    size: Int,
    acc: String = ""
  ): Int =
    if s"${dataStreamBuffer.head}${acc.take(size - 1)}".distinct.length == size
    then
      acc.length + 1
    else
      findMarkerLocation(
        dataStreamBuffer.tail,
        size,
        s"${dataStreamBuffer.head}$acc"
      )

  override lazy val pt1: Int =
    findMarkerLocation(input, 4)

  override lazy val pt2: Int =
    findMarkerLocation(input, 14)
