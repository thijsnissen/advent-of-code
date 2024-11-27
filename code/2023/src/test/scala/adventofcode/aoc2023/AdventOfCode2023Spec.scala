package adventofcode
package aoc2023

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2023Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(142)(Day01.pt1)
      assertResult(225)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(56397)(Day01.pt1)
      assertResult(55701)(Day01.pt2)

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(8)(Day02.pt1)
      assertResult(2286)(Day02.pt2)

    if Day02.getEnv == Prod then
      assertResult(2169)(Day02.pt1)
      assertResult(60948)(Day02.pt2)

  test("Day03"):
    if Day03.getEnv == Test then
      assertResult(4361)(Day03.pt1)
      assertResult(467835)(Day03.pt2)

    if Day03.getEnv == Prod then
      assertResult(509115)(Day03.pt1)
      assertResult(75220503)(Day03.pt2)

  test("Day04"):
    if Day04.getEnv == Test then
      assertResult(13)(Day04.pt1)
      assertResult(30)(Day04.pt2)

    if Day04.getEnv == Prod then
      assertResult(26346)(Day04.pt1)
      assertResult(8467762)(Day04.pt2)

  test("Day05"):
    if Day05.getEnv == Test then
      assertResult(35)(Day05.pt1)
      assertResult(46)(Day05.pt2)

    if Day05.getEnv == Prod then
      assertResult(993500720)(Day05.pt1)
      assertResult(4917124)(Day05.pt2)

  test("Day06"):
    if Day06.getEnv == Test then
      assertResult(288)(Day06.pt1)
      assertResult(71503)(Day06.pt2)

    if Day06.getEnv == Prod then
      assertResult(1710720)(Day06.pt1)
      assertResult(35349468)(Day06.pt2)

  test("Day07"):
    if Day07.getEnv == Test then
      assertResult(6440)(Day07.pt1)
      assertResult(5905)(Day07.pt2)

    if Day07.getEnv == Prod then
      assertResult(249390788)(Day07.pt1)
      assertResult(248750248)(Day07.pt2)

  test("Day08"):
    if Day08.getEnv == Test then
      assertResult(3)(Day08.pt1)
      assertResult(6)(Day08.pt2)

    if Day08.getEnv == Prod then
      assertResult(16897)(Day08.pt1)
      assertResult(16563603485021L)(Day08.pt2)

  test("Day09"):
    if Day09.getEnv == Test then
      assertResult(114)(Day09.pt1)
      assertResult(2)(Day09.pt2)

    if Day09.getEnv == Prod then
      assertResult(1953784198)(Day09.pt1)
      assertResult(957)(Day09.pt2)

  test("Day10"):
    if Day10.getEnv == Test then
      assertResult(80)(Day10.pt1)
      assertResult(10)(Day10.pt2)

    if Day10.getEnv == Prod then
      assertResult(6773)(Day10.pt1)
      assertResult(493)(Day10.pt2)

  test("Day11"):
    if Day11.getEnv == Test then
      assertResult(374L)(Day11.pt1)
      assertResult(82000210L)(Day11.pt2)

    if Day11.getEnv == Prod then
      assertResult(9403026L)(Day11.pt1)
      assertResult(543018317006L)(Day11.pt2)

  test("Day12"):
    if Day12.getEnv == Test then
      assertResult(21)(Day12.pt1)
      assertResult(525152)(Day12.pt2)

    if Day12.getEnv == Prod then
      assertResult(7916)(Day12.pt1)
      assertResult(37366887898686L)(Day12.pt2)

  test("Day13"):
    if Day13.getEnv == Test then
      assertResult(405)(Day13.pt1)
      assertResult(400)(Day13.pt2)

    if Day13.getEnv == Prod then
      assertResult(41859)(Day13.pt1)
      assertResult(30842)(Day13.pt2)

  test("Day14"):
    if Day14.getEnv == Test then
      assertResult(136)(Day14.pt1)
      assertResult(64)(Day14.pt2)

    if Day14.getEnv == Prod then
      assertResult(105784)(Day14.pt1)
      assertResult(91286)(Day14.pt2)

  test("Day15"):
    if Day15.getEnv == Test then
      assertResult(1320)(Day15.pt1)
      assertResult(145)(Day15.pt2)

    if Day15.getEnv == Prod then
      assertResult(511215)(Day15.pt1)
      assertResult(236057)(Day15.pt2)

  test("Day16"):
    if Day16.getEnv == Test then
      assertResult(46)(Day16.pt1)
      assertResult(51)(Day16.pt2)

    if Day16.getEnv == Prod then
      assertResult(7517)(Day16.pt1)
      assertResult(7741)(Day16.pt2)

  test("Day17"):
    if Day17.getEnv == Test then
      assertResult(102)(Day17.pt1)
      assertResult(94)(Day17.pt2)

    if Day17.getEnv == Prod then
      assertResult(665)(Day17.pt1)
      assertResult(809)(Day17.pt2)

  test("Day18"):
    if Day18.getEnv == Test then
      assertResult(62)(Day18.pt1)
      assertResult(952408144115L)(Day18.pt2)

    if Day18.getEnv == Prod then
      assertResult(53844)(Day18.pt1)
      assertResult(42708339569950L)(Day18.pt2)

  test("Day19"):
    if Day19.getEnv == Test then
      assertResult(19114)(Day19.pt1)
      assertResult(167409079868000L)(Day19.pt2)

    if Day19.getEnv == Prod then
      assertResult(425811)(Day19.pt1)
      assertResult(131796824371749L)(Day19.pt2)

  test("Day20"):
    if Day20.getEnv == Test then
      assertResult(11687500)(Day20.pt1)

    if Day20.getEnv == Prod then
      assertResult(711650489)(Day20.pt1)
      assertResult(219388737656593L)(Day20.pt2)

  test("Day21"):
    if Day21.getEnv == Test then
      assertResult(16)(Day21.pt1)
      assertResult(6536)(Day21.pt2)

    if Day21.getEnv == Prod then
      assertResult(3782)(Day21.pt1)
      assertResult(630661863455116L)(Day21.pt2)

  test("Day22"):
    if Day22.getEnv == Test then
      assertResult(5)(Day22.pt1)
      assertResult(7)(Day22.pt2)

    if Day22.getEnv == Prod then
      assertResult(391)(Day22.pt1)
      assertResult(69601)(Day22.pt2)

  test("Day23"):
    if Day23.getEnv == Test then
      assertResult(94)(Day23.pt1)
      assertResult(154)(Day23.pt2)

    if Day23.getEnv == Prod then
      assertResult(2162)(Day23.pt1)
      assertResult(6334)(Day23.pt2)

  test("Day24"):
    if Day24.getEnv == Test then
      assertResult(2)(Day24.pt1)
      assertResult(47)(Day24.pt2)

    if Day24.getEnv == Prod then
      assertResult(13149)(Day24.pt1)
      assertResult(BigDecimal(1033770143421619L))(Day24.pt2)

  test("Day25"):
    if Day25.getEnv == Test then
      assertResult(54)(Day25.pt1)

    if Day25.getEnv == Prod then
      assertResult(614655)(Day25.pt1)
