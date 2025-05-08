package adventofcode
package aoc2017

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2017Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(3)(Day01.pt1)
      assertResult(0)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(1089)(Day01.pt1)
      assertResult(1156)(Day01.pt2)

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(18)(Day02.pt1)
      assertResult(9)(Day02.pt2)

    if Day02.getEnv == Prod then
      assertResult(43074)(Day02.pt1)
      assertResult(280)(Day02.pt2)
