package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day05 extends AdventOfCode(Prod):
  @annotation.tailrec
  def removeReactingUnits(input: String, acc: String = " "): String =
    if input.isEmpty then
      acc.reverse.trim
    else if input.head.toLower == acc.head.toLower && input.head != acc.head
    then
      removeReactingUnits(input.tail, acc.drop(1))
    else
      removeReactingUnits(input.tail, s"${input.head}$acc")

  def shortestPolymer(input: String) =
    ('a' to 'z')
      .map(x => input.filterNot(_.toLower == x))
      .map(removeReactingUnits(_).length).min

  lazy val pt1: Int =
    removeReactingUnits(input).length

  lazy val pt2: Int =
    shortestPolymer(input)

  answer(1)(pt1)

  answer(2)(pt2)
