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

  test("Day04"):
    if Day04.getEnv == Test then
      assertResult(18)(Day04.pt1)
      assertResult(9)(Day04.pt2)

    if Day04.getEnv == Prod then
      assertResult(2646)(Day04.pt1)
      assertResult(2000)(Day04.pt2)

  test("Day05"):
    if Day05.getEnv == Test then
      assertResult(143)(Day05.pt1)
      assertResult(123)(Day05.pt2)

    if Day05.getEnv == Prod then
      assertResult(4662)(Day05.pt1)
      assertResult(5900)(Day05.pt2)

  test("Day06"):
    if Day06.getEnv == Test then
      assertResult(41)(Day06.pt1)
      assertResult(6)(Day06.pt2)

    if Day06.getEnv == Prod then
      assertResult(5208)(Day06.pt1)
      assertResult(1972)(Day06.pt2)

  test("Day07"):
    if Day07.getEnv == Test then
      assertResult(3749)(Day07.pt1)
      assertResult(11387)(Day07.pt2)

    if Day07.getEnv == Prod then
      assertResult(1153997401072L)(Day07.pt1)
      assertResult(97902809384118L)(Day07.pt2)

  test("Day08"):
    if Day08.getEnv == Test then
      assertResult(14)(Day08.pt1)
      assertResult(34)(Day08.pt2)

    if Day08.getEnv == Prod then
      assertResult(276)(Day08.pt1)
      assertResult(991)(Day08.pt2)

  test("Day09"):
    if Day09.getEnv == Test then
      assertResult(1928)(Day09.pt1)
      assertResult(2858)(Day09.pt2)

    if Day09.getEnv == Prod then
      assertResult(6283404590840L)(Day09.pt1)
      assertResult(6304576012713L)(Day09.pt2)

  test("Day10"):
    if Day10.getEnv == Test then
      assertResult(36)(Day10.pt1)
      assertResult(81)(Day10.pt2)

    if Day10.getEnv == Prod then
      assertResult(782)(Day10.pt1)
      assertResult(1694)(Day10.pt2)

  test("Day11"):
    if Day11.getEnv == Test then
      assertResult(55312)(Day11.pt1)
      assertResult(65601038650482L)(Day11.pt2)

    if Day11.getEnv == Prod then
      assertResult(203228)(Day11.pt1)
      assertResult(240884656550923L)(Day11.pt2)
