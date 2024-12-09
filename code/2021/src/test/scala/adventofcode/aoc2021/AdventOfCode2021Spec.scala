package adventofcode
package aoc2021

import adventofcode.utilities.AdventOfCode.*
import org.scalatest.funsuite.AnyFunSuite

class AdventOfCode2021Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(7)(Day01.pt1)
      assertResult(5)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(1475)(Day01.pt1)
      assertResult(1516)(Day01.pt2)

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(150)(Day02.pt1)
      assertResult(900)(Day02.pt2)

    if Day02.getEnv == Prod then
      assertResult(2147104)(Day02.pt1)
      assertResult(2044620088)(Day02.pt2)

  test("Day03"):
    if Day03.getEnv == Test then
      assertResult(198)(Day03.pt1)
      assertResult(230)(Day03.pt2)

    if Day03.getEnv == Prod then
      assertResult(3901196)(Day03.pt1)
      assertResult(4412188)(Day03.pt2)

  test("Day04"):
    if Day04.getEnv == Test then
      assertResult(4512)(Day04.pt1)
      assertResult(1924)(Day04.pt2)

    if Day04.getEnv == Prod then
      assertResult(2745)(Day04.pt1)
      assertResult(6594)(Day04.pt2)

  test("Day05"):
    if Day05.getEnv == Test then
      assertResult(5)(Day05.pt1)
      assertResult(12)(Day05.pt2)

    if Day05.getEnv == Prod then
      assertResult(6856)(Day05.pt1)
      assertResult(20666)(Day05.pt2)

  test("Day06"):
    if Day06.getEnv == Test then
      assertResult(5934L)(Day06.pt1)
      assertResult(26984457539L)(Day06.pt2)

    if Day06.getEnv == Prod then
      assertResult(352872L)(Day06.pt1)
      assertResult(1604361182149L)(Day06.pt2)

  test("Day07"):
    if Day07.getEnv == Test then
      assertResult(37)(Day07.pt1)
      assertResult(168)(Day07.pt2)

    if Day07.getEnv == Prod then
      assertResult(336701)(Day07.pt1)
      assertResult(95167302)(Day07.pt2)
