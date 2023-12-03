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

  test("Day03"):
    if Day03.env == Test then
      assertResult(4361)(Day03.pt1)   // [18ms]
      assertResult(467835)(Day03.pt2) // [4ms]

    if Day03.env == Prod then
      assertResult(509115)(Day03.pt1)   // [60ms]
      assertResult(75220503)(Day03.pt2) // [698ms]
