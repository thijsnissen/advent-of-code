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
      assertResult(80)(Day10.pt1) // [0ms]
      assertResult(10)(Day10.pt2) // [0ms]

    if Day10.getEnv == Prod then
      assertResult(6773)(Day10.pt1) // [0ms]
      assertResult(???)(Day10.pt2) // [0ms]

  test("Day11"):
    if Day11.getEnv == Test then
      assertResult(374L)(Day11.pt1) // [13ms]
      assertResult(8410L)(Day11.pt2) // [1ms]

    if Day11.getEnv == Prod then
      assertResult(9403026L)(Day11.pt1) // [111ms]
      assertResult(543018317006L)(Day11.pt2) // [75ms]
