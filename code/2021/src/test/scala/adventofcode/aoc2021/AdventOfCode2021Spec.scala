package adventofcode
package aoc2021

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.Env.*

class AdventOfCode2021Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.env == Test then
      assertResult(7)(Day01.pt1) // [0ms]
      assertResult(5)(Day01.pt2) // [0ms]

    if Day01.env == Prod then
      assertResult(1475)(Day01.pt1) // [1ms]
      assertResult(1516)(Day01.pt2) // [2ms]

  test("Day02"):
    if Day02.env == Test then
      assertResult(150)(Day02.pt1) // [0ms]
      assertResult(900)(Day02.pt2) // [0ms]

    if Day02.env == Prod then
      assertResult(2147104)(Day02.pt1)    // [0ms]
      assertResult(2044620088)(Day02.pt2) // [1ms]
