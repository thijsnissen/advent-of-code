package adventofcode
package aoc2024

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2024Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(11)(Day01.pt1)
      assertResult(31)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(1320851)(Day01.pt1)
      assertResult(26859182)(Day01.pt2)

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(2)(Day02.pt1)
      assertResult(4)(Day02.pt2)

    if Day02.getEnv == Prod then
      assertResult(326)(Day02.pt1)
      assertResult(381)(Day02.pt2)

  test("Day03"):
    if Day03.getEnv == Test then
      assertResult(161)(Day03.pt1)
      assertResult(48)(Day03.pt2)

    if Day03.getEnv == Prod then
      assertResult(178538786)(Day03.pt1)
      assertResult(102467299)(Day03.pt2)
