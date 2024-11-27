package adventofcode
package aoc2024

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2024Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(0)(Day01.pt1)
      assertResult(0)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(0)(Day01.pt1)
      assertResult(0)(Day01.pt2)
