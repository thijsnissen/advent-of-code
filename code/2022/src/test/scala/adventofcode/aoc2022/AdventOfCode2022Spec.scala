package adventofcode
package aoc2022

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2022Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(24000)(Day01.pt1)
      assertResult(45000)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(68923)(Day01.pt1)
      assertResult(200044)(Day01.pt2)

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(15)(Day02.pt1)
      assertResult(12)(Day02.pt2)

    if Day02.getEnv == Prod then
      assertResult(15422)(Day02.pt1)
      assertResult(15442)(Day02.pt2)

  test("Day03"):
    if Day03.getEnv == Test then
      assertResult(157)(Day03.pt1)
      assertResult(70)(Day03.pt2)

    if Day03.getEnv == Prod then
      assertResult(8298)(Day03.pt1)
      assertResult(2708)(Day03.pt2)

  test("Day04"):
    if Day04.getEnv == Test then
      assertResult(2)(Day04.pt1)
      assertResult(4)(Day04.pt2)

    if Day04.getEnv == Prod then
      assertResult(424)(Day04.pt1)
      assertResult(804)(Day04.pt2)

  test("Day05"):
    if Day05.getEnv == Test then
      assertResult("CMZ")(Day05.pt1)
      assertResult("MCD")(Day05.pt2)

    if Day05.getEnv == Prod then
      assertResult("LBLVVTVLP")(Day05.pt1)
      assertResult("TPFFBDRJD")(Day05.pt2)

  test("Day06"):
    if Day06.getEnv == Test then
      assertResult(7)(Day06.pt1)
      assertResult(19)(Day06.pt2)

    if Day06.getEnv == Prod then
      assertResult(1850)(Day06.pt1)
      assertResult(2823)(Day06.pt2)

  test("Day07"):
    if Day07.getEnv == Test then
      assertResult(95437)(Day07.pt1)
      assertResult(24933642)(Day07.pt2)

    if Day07.getEnv == Prod then
      assertResult(1501149)(Day07.pt1)
      assertResult(10096985)(Day07.pt2)

  test("Day08"):
    if Day08.getEnv == Test then
      assertResult(21)(Day08.pt1)
      assertResult(8)(Day08.pt2)

    if Day08.getEnv == Prod then
      assertResult(1829)(Day08.pt1)
      assertResult(291840)(Day08.pt2)

  test("Day09"):
    if Day09.getEnv == Test then
      assertResult(13)(Day09.pt1)
      assertResult(1)(Day09.pt2)

    if Day09.getEnv == Prod then
      assertResult(5981)(Day09.pt1)
      assertResult(2352)(Day09.pt2)

  test("Day10"):
    if Day10.getEnv == Test then
      assertResult(13140)(Day10.pt1)
      assertResult(
        """
					|■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . .
					|■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ .
					|■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . .
					|■ ■ ■ ■ ■ . . . . . ■ ■ ■ ■ ■ . . . . . ■ ■ ■ ■ ■ . . . . . ■ ■ ■ ■ ■ . . . . .
					|■ ■ ■ ■ ■ ■ . . . . . . ■ ■ ■ ■ ■ ■ . . . . . . ■ ■ ■ ■ ■ ■ . . . . . . ■ ■ ■ ■
					|■ ■ ■ ■ ■ ■ ■ . . . . . . . ■ ■ ■ ■ ■ ■ ■ . . . . . . . ■ ■ ■ ■ ■ ■ ■ . . . . .
					|""".stripMargin
      )(Day10.pt2)

    if Day10.getEnv == Prod then
      assertResult(14340)(Day10.pt1)
      assertResult(
        """
					|■ ■ ■ . . . ■ ■ . . ■ ■ ■ . . . . ■ ■ . . ■ ■ . . ■ ■ ■ . . ■ . . ■ . ■ ■ ■ . .
					|■ . . ■ . ■ . . ■ . ■ . . ■ . . . . ■ . ■ . . ■ . ■ . . ■ . ■ . . ■ . ■ . . ■ .
					|■ . . ■ . ■ . . ■ . ■ . . ■ . . . . ■ . ■ . . . . ■ ■ ■ . . ■ ■ ■ ■ . ■ . . ■ .
					|■ ■ ■ . . ■ ■ ■ ■ . ■ ■ ■ . . . . . ■ . ■ . . . . ■ . . ■ . ■ . . ■ . ■ ■ ■ . .
					|■ . . . . ■ . . ■ . ■ . . . . ■ . . ■ . ■ . . ■ . ■ . . ■ . ■ . . ■ . ■ . . . .
					|■ . . . . ■ . . ■ . ■ . . . . . ■ ■ . . . ■ ■ . . ■ ■ ■ . . ■ . . ■ . ■ . . . .
					|""".stripMargin
      )(Day10.pt2)

  test("Day11"):
    if Day11.getEnv == Test then
      assertResult(10605L)(Day11.pt1)
      assertResult(2713310158L)(Day11.pt2)

    if Day11.getEnv == Prod then
      assertResult(57348L)(Day11.pt1)
      assertResult(14106266886L)(Day11.pt2)

  test("Day12"):
    if Day12.getEnv == Test then
      assertResult(31)(Day12.pt1)
      assertResult(29)(Day12.pt2)

    if Day12.getEnv == Prod then
      assertResult(517)(Day12.pt1)
      assertResult(512)(Day12.pt2)
