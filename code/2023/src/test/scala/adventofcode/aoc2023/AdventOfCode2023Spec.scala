package adventofcode
package aoc2023

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2023Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(142)(Day01.pt1) // [2ms]
      assertResult(225)(Day01.pt2) // [2ms]

    if Day01.getEnv == Prod then
      assertResult(56397)(Day01.pt1) // [5ms]
      assertResult(55701)(Day01.pt2) // [18ms]

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(8)(Day02.pt1)    // [2ms]
      assertResult(2286)(Day02.pt2) // [6ms]

    if Day02.getEnv == Prod then
      assertResult(2169)(Day02.pt1)  // [3ms]
      assertResult(60948)(Day02.pt2) // [9ms]

  test("Day03"):
    if Day03.getEnv == Test then
      assertResult(4361)(Day03.pt1)   // [18ms]
      assertResult(467835)(Day03.pt2) // [4ms]

    if Day03.getEnv == Prod then
      assertResult(509115)(Day03.pt1)   // [60ms]
      assertResult(75220503)(Day03.pt2) // [698ms]

  test("Day04"):
    if Day04.getEnv == Test then
      assertResult(13)(Day04.pt1) // [4ms]
      assertResult(30)(Day04.pt2) // [6ms]

    if Day04.getEnv == Prod then
      assertResult(26346)(Day04.pt1)   // [8ms]
      assertResult(8467762)(Day04.pt2) // [582ms]

  test("Day05"):
    if Day05.getEnv == Test then
      assertResult(35)(Day05.pt1) // [12ms]
      assertResult(46)(Day05.pt2) // [2ms]

    if Day05.getEnv == Prod then
      assertResult(993500720)(Day05.pt1) // [15ms]
      assertResult(4917124)(Day05.pt2)   // [6ms]

  test("Day06"):
    if Day06.getEnv == Test then
      assertResult(288)(Day06.pt1)   // [0ms]
      assertResult(71503)(Day06.pt2) // [3ms]

    if Day06.getEnv == Prod then
      assertResult(1710720)(Day06.pt1)  // [1ms]
      assertResult(35349468)(Day06.pt2) // [28ms]

  test("Day07"):
    if Day07.getEnv == Test then
      assertResult(6440)(Day07.pt1) // [2ms]
      assertResult(5905)(Day07.pt2) // [3ms]

    if Day07.getEnv == Prod then
      assertResult(249390788)(Day07.pt1) // [9ms]
      assertResult(248750248)(Day07.pt2) // [18ms]

  test("Day08"):
    if Day08.getEnv == Test then
      assertResult(3)(Day08.pt1) // [0ms]
      assertResult(6)(Day08.pt2) // [5ms]

    if Day08.getEnv == Prod then
      assertResult(16897)(Day08.pt1)           // [4ms]
      assertResult(16563603485021L)(Day08.pt2) // [21ms]

  test("Day09"):
    if Day09.getEnv == Test then
      assertResult(114)(Day09.pt1) // [5ms]
      assertResult(2)(Day09.pt2)   // [1ms]

    if Day09.getEnv == Prod then
      assertResult(1953784198)(Day09.pt1) // [36ms]
      assertResult(957)(Day09.pt2)        // [16ms]

  test("Day10"):
    if Day10.getEnv == Test then
      assertResult(80)(Day10.pt1) // [24ms]
      assertResult(10)(Day10.pt2) // [12ms]

    if Day10.getEnv == Prod then
      assertResult(6773)(Day10.pt1) // [745ms]
      assertResult(493)(Day10.pt2)  // [2281ms]

  test("Day11"):
    if Day11.getEnv == Test then
      assertResult(374L)(Day11.pt1)      // [13ms]
      assertResult(82000210L)(Day11.pt2) // [1ms]

    if Day11.getEnv == Prod then
      assertResult(9403026L)(Day11.pt1)      // [111ms]
      assertResult(543018317006L)(Day11.pt2) // [75ms]

  test("Day12"):
    if Day12.getEnv == Test then
      assertResult(21)(Day12.pt1)     // [6ms]
      assertResult(525152)(Day12.pt2) // [5ms]

    if Day12.getEnv == Prod then
      assertResult(7916)(Day12.pt1)            // [29ms]
      assertResult(37366887898686L)(Day12.pt2) // [242ms]

  test("Day13"):
    if Day13.getEnv == Test then
      assertResult(405)(Day13.pt1) // [6ms]
      assertResult(400)(Day13.pt2) // [3ms]

    if Day13.getEnv == Prod then
      assertResult(41859)(Day13.pt1) // [19ms]
      assertResult(30842)(Day13.pt2) // [16ms]

  test("Day14"):
    if Day14.getEnv == Test then
      assertResult(136)(Day14.pt1) // [4ms]
      assertResult(64)(Day14.pt2)  // [10ms]

    if Day14.getEnv == Prod then
      assertResult(105784)(Day14.pt1) // [31ms]
      assertResult(91286)(Day14.pt2)  // [4245ms]

  test("Day15"):
    if Day15.getEnv == Test then
      assertResult(1320)(Day15.pt1) // [2ms]
      assertResult(145)(Day15.pt2)  // [11ms]

    if Day15.getEnv == Prod then
      assertResult(511215)(Day15.pt1) // [60ms]
      assertResult(236057)(Day15.pt2) // [40ms]

  test("Day16"):
    if Day16.getEnv == Test then
      assertResult(46)(Day16.pt1) // [9ms]
      assertResult(51)(Day16.pt2) // [11ms]

    if Day16.getEnv == Prod then
      assertResult(7517)(Day16.pt1) // [43ms]
      assertResult(7741)(Day16.pt2) // [547ms]

  test("Day17"):
    if Day17.getEnv == Test then
      assertResult(102)(Day17.pt1) // [63ms]
      assertResult(94)(Day17.pt2)  // [12ms]

    if Day17.getEnv == Prod then
      assertResult(665)(Day17.pt1) // [1886ms]
      assertResult(809)(Day17.pt2) // [4247ms]

  test("Day18"):
    if Day18.getEnv == Test then
      assertResult(62)(Day18.pt1)            // [2ms]
      assertResult(952408144115L)(Day18.pt2) // [0ms]

    if Day18.getEnv == Prod then
      assertResult(53844)(Day18.pt1)           // [6ms]
      assertResult(42708339569950L)(Day18.pt2) // [2ms]

  test("Day19"):
    if Day19.getEnv == Test then
      assertResult(19114)(Day19.pt1)            // [1ms]
      assertResult(167409079868000L)(Day19.pt2) // [1ms]

    if Day19.getEnv == Prod then
      assertResult(425811)(Day19.pt1)           // [4ms]
      assertResult(131796824371749L)(Day19.pt2) // [8ms]

  test("Day20"):
    if Day20.getEnv == Test then
      assertResult(11687500)(Day20.pt1) // [18ms]

    if Day20.getEnv == Prod then
      assertResult(711650489)(Day20.pt1)        // [46ms]
      assertResult(219388737656593L)(Day20.pt2) // [110ms]

  test("Day21"):
    if Day21.getEnv == Test then
      assertResult(16)(Day21.pt1)   // [10ms]
      assertResult(6536)(Day21.pt2) // [57ms]

    if Day21.getEnv == Prod then
      assertResult(3782)(Day21.pt1)             // [61ms]
      assertResult(630661863455116L)(Day21.pt2) // [535ms]

  test("Day22"):
    if Day22.getEnv == Test then
      assertResult(5)(Day22.pt1) // [7ms]
      assertResult(7)(Day22.pt2) // [4ms]

    if Day22.getEnv == Prod then
      assertResult(391)(Day22.pt1)   // [80ms]
      assertResult(69601)(Day22.pt2) // [145ms]

  test("Day23"):
    if Day23.getEnv == Test then
      assertResult(94)(Day23.pt1)  // [21ms]
      assertResult(154)(Day23.pt2) // [6ms]

    if Day23.getEnv == Prod then
      assertResult(2162)(Day23.pt1) // [65ms]
      assertResult(6334)(Day23.pt2) // [14184ms]

  test("Day24"):
    if Day24.getEnv == Test then
      assertResult(2)(Day24.pt1)  // [0ms]
      assertResult(47)(Day24.pt2) // [0ms]

    if Day24.getEnv == Prod then
      assertResult(13149)(Day24.pt1)                         // [0ms]
      assertResult(BigDecimal(1033770143421619L))(Day24.pt2) // [0ms]

  test("Day25"):
    if Day25.getEnv == Test then
      assertResult(54)(Day25.pt1) // [15ms]

    if Day25.getEnv == Prod then
      assertResult(614655)(Day25.pt1) // [18956ms]
