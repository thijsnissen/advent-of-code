package adventofcode
package aoc2023

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.Env.*

class AdventOfCode2023Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.env == Test then
      assertResult(???)(Day01.pt1) // [0ms]
      assertResult(???)(Day01.pt2) // [0ms]

    if Day01.env == Prod then
      assertResult(???)(Day01.pt1) // [0ms]
      assertResult(???)(Day01.pt2) // [0ms]
