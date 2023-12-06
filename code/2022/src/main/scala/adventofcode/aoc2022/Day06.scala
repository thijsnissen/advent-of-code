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

  lazy val pt1: Int =
    findMarkerLocation(input, 4)

  lazy val pt2: Int =
    findMarkerLocation(input, 14)

  answer(1)(pt1)

  answer(2)(pt2)
