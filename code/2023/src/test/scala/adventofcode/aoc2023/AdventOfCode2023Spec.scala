package adventofcode
package aoc2023

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.Env.*

class AdventOfCode2023Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.env == Test then
      assertResult(142)(Day01.pt1) // [2ms]
      assertResult(225)(Day01.pt2) // [2ms]

    if Day01.env == Prod then
      assertResult(56397)(Day01.pt1) // [5ms]
      assertResult(55701)(Day01.pt2) // [18ms]

  test("Day02"):
    if Day02.env == Test then
      assertResult(8)(Day02.pt1)    // [2ms]
      assertResult(2286)(Day02.pt2) // [6ms]

    if Day02.env == Prod then
      assertResult(2169)(Day02.pt1)  // [3ms]
      assertResult(60948)(Day02.pt2) // [9ms]
