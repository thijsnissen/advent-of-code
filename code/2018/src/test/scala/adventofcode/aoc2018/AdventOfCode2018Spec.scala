package adventofcode
package aoc2018

import org.scalatest.funsuite.AnyFunSuite
import utilities.AdventOfCode.*

class AdventOfCode2018Spec extends AnyFunSuite:
  test("Day01"):
    if Day01.getEnv == Test then
      assertResult(4)(Day01.pt1)
      assertResult(10)(Day01.pt2)

    if Day01.getEnv == Prod then
      assertResult(585)(Day01.pt1)
      assertResult(83173)(Day01.pt2)

  test("Day02"):
    if Day02.getEnv == Test then
      assertResult(0)(Day02.pt1)
      assertResult("fgij")(Day02.pt2)

    if Day02.getEnv == Prod then
      assertResult(7105)(Day02.pt1)
      assertResult("omlvgdokxfncvqyersasjziup")(Day02.pt2)

  test("Day03"):
    if Day03.getEnv == Test then
      assertResult(4)(Day03.pt1)
      assertResult(3)(Day03.pt2)

    if Day03.getEnv == Prod then
      assertResult(114946)(Day03.pt1)
      assertResult(877)(Day03.pt2)

  test("Day04"):
    if Day04.getEnv == Test then
      assertResult(240)(Day04.pt1)
      assertResult(4455)(Day04.pt2)

    if Day04.getEnv == Prod then
      assertResult(76357)(Day04.pt1)
      assertResult(41668)(Day04.pt2)

  test("Day05"):
    if Day05.getEnv == Test then
      assertResult(10)(Day05.pt1)
      assertResult(4)(Day05.pt2)

    if Day05.getEnv == Prod then
      assertResult(9078)(Day05.pt1)
      assertResult(5698)(Day05.pt2)

  test("Day06"):
    if Day06.getEnv == Test then
      assertResult(17)(Day06.pt1)
      assertResult(16)(Day06.pt2)

    if Day06.getEnv == Prod then
      assertResult(3989)(Day06.pt1)
      assertResult(49715)(Day06.pt2)

  test("Day07"):
    if Day07.getEnv == Test then
      assertResult("CABDFE")(Day07.pt1)
      assertResult(253)(Day07.pt2)

    if Day07.getEnv == Prod then
      assertResult("IBJTUWGFKDNVEYAHOMPCQRLSZX")(Day07.pt1)
      assertResult(1118)(Day07.pt2)

  test("Day08"):
    if Day08.getEnv == Test then
      assertResult(138)(Day08.pt1)
      assertResult(66)(Day08.pt2)

    if Day08.getEnv == Prod then
      assertResult(37439)(Day08.pt1)
      assertResult(20815)(Day08.pt2)

  test("Day09"):
    if Day09.getEnv == Test then
      assertResult(32)(Day09.pt1)
      assertResult(22563)(Day09.pt2)

    if Day09.getEnv == Prod then
      assertResult(388024)(Day09.pt1)
      assertResult(3180929875L)(Day09.pt2)

  test("Day10"):
    if Day10.getEnv == Test then
      assertResult(
        """
					|■ . . . ■ . . ■ ■ ■
					|■ . . . ■ . . . ■ .
					|■ . . . ■ . . . ■ .
					|■ ■ ■ ■ ■ . . . ■ .
					|■ . . . ■ . . . ■ .
					|■ . . . ■ . . . ■ .
					|■ . . . ■ . . . ■ .
					|■ . . . ■ . . ■ ■ ■"""
          .stripMargin
      )(Day10.pt1)
      assertResult(3)(Day10.pt2)

    if Day10.getEnv == Prod then
      assertResult(
        """
					|■ ■ ■ ■ ■ . . . ■ ■ ■ ■ ■ . . . ■ . . . . ■ . . ■ . . . . ■ . . ■ . . . . ■ . . ■ ■ ■ ■ ■ ■ . . ■ ■ ■ ■ ■ ■ . . ■ ■ ■ ■ ■ .
					|■ . . . . ■ . . ■ . . . . ■ . . ■ ■ . . . ■ . . ■ ■ . . . ■ . . ■ . . . . ■ . . ■ . . . . . . . . . . . . ■ . . ■ . . . . ■
					|■ . . . . ■ . . ■ . . . . ■ . . ■ ■ . . . ■ . . ■ ■ . . . ■ . . . ■ . . ■ . . . ■ . . . . . . . . . . . . ■ . . ■ . . . . ■
					|■ . . . . ■ . . ■ . . . . ■ . . ■ . ■ . . ■ . . ■ . ■ . . ■ . . . ■ . . ■ . . . ■ . . . . . . . . . . . ■ . . . ■ . . . . ■
					|■ ■ ■ ■ ■ . . . ■ ■ ■ ■ ■ . . . ■ . ■ . . ■ . . ■ . ■ . . ■ . . . . ■ ■ . . . . ■ ■ ■ ■ ■ . . . . . . ■ . . . . ■ ■ ■ ■ ■ .
					|■ . . ■ . . . . ■ . . . . . . . ■ . . ■ . ■ . . ■ . . ■ . ■ . . . . ■ ■ . . . . ■ . . . . . . . . . ■ . . . . . ■ . . ■ . .
					|■ . . . ■ . . . ■ . . . . . . . ■ . . ■ . ■ . . ■ . . ■ . ■ . . . ■ . . ■ . . . ■ . . . . . . . . ■ . . . . . . ■ . . . ■ .
					|■ . . . ■ . . . ■ . . . . . . . ■ . . . ■ ■ . . ■ . . . ■ ■ . . . ■ . . ■ . . . ■ . . . . . . . ■ . . . . . . . ■ . . . ■ .
					|■ . . . . ■ . . ■ . . . . . . . ■ . . . ■ ■ . . ■ . . . ■ ■ . . ■ . . . . ■ . . ■ . . . . . . . ■ . . . . . . . ■ . . . . ■
					|■ . . . . ■ . . ■ . . . . . . . ■ . . . . ■ . . ■ . . . . ■ . . ■ . . . . ■ . . ■ . . . . . . . ■ ■ ■ ■ ■ ■ . . ■ . . . . ■"""
          .stripMargin
      )(Day10.pt1)
      assertResult(10946)(Day10.pt2)

  test("Day11"):
    if Day11.getEnv == Test then
      assertResult("33,45")(Day11.pt1)
      assertResult("90,269,16")(Day11.pt2)

    if Day11.getEnv == Prod then
      assertResult("243,27")(Day11.pt1)
      assertResult("284,172,12")(Day11.pt2)

  test("Day12"):
    if Day12.getEnv == Test then
      assertResult(325)(Day12.pt1)
      assertResult(999999999374L)(Day12.pt2)

    if Day12.getEnv == Prod then
      assertResult(3915)(Day12.pt1)
      assertResult(4900000001793L)(Day12.pt2)

  test("Day13"):
    if Day13.getEnv == Test then
      assertResult("7,3")(Day13.pt1)
      assertResult("7,3")(Day13.pt2)

    if Day13.getEnv == Prod then
      assertResult("103,85")(Day13.pt1)
      assertResult("88,64")(Day13.pt2)

  test("Day14"):
    if Day14.getEnv == Test then
      assertResult("0124515891")(Day14.pt1)
      assertResult(9)(Day14.pt2)

    if Day14.getEnv == Prod then
      assertResult("7861362411")(Day14.pt1)
      assertResult(20203532)(Day14.pt2)

  test("Day15"):
    if Day15.getEnv == Test then
      assertResult(27730)(Day15.pt1)
      assertResult(4988)(Day15.pt2)

    if Day15.getEnv == Prod then
      assertResult(181522)(Day15.pt1)
      assertResult(68324)(Day15.pt2)

  test("Day16"):
    if Day16.getEnv == Test then
      assertResult(1)(Day16.pt1)

    if Day16.getEnv == Prod then
      assertResult(544)(Day16.pt1)
      assertResult(600)(Day16.pt2)

  test("Day17"):
    if Day17.getEnv == Test then
      assertResult(57)(Day17.pt1)
      assertResult(29)(Day17.pt2)

    if Day17.getEnv == Prod then
      assertResult(33242)(Day17.pt1)
      assertResult(27256)(Day17.pt2)

  test("Day18"):
    if Day18.getEnv == Test then
      assertResult(1147)(Day18.pt1)
      assertResult(0)(Day18.pt2)

    if Day18.getEnv == Prod then
      assertResult(519552)(Day18.pt1)
      assertResult(165376)(Day18.pt2)

  test("Day19"):
    if Day19.getEnv == Test then
      assertResult(7)(Day19.pt1)

    if Day19.getEnv == Prod then
      assertResult(2304)(Day19.pt1)
      assertResult(28137600)(Day19.pt2)

  test("Day20"):
    if Day20.getEnv == Test then
      assertResult(23)(Day20.pt1)

    if Day20.getEnv == Prod then
      assertResult(4247)(Day20.pt1)
      assertResult(8356)(Day20.pt2)

  test("Day21"):
    if Day21.getEnv == Prod then
      assertResult(2525738)(Day21.pt1)
      assertResult(11316540)(Day21.pt2)

  test("Day22"):
    if Day22.getEnv == Test then
      assertResult(114)(Day22.pt1)
      assertResult(45)(Day22.pt2)

    if Day22.getEnv == Prod then
      assertResult(7915)(Day22.pt1)
      assertResult(980)(Day22.pt2)

  test("Day23"):
    if Day23.getEnv == Test then
      assertResult(6)(Day23.pt1)
      assertResult(36)(Day23.pt2)

    if Day23.getEnv == Prod then
      assertResult(943)(Day23.pt1)
      assertResult(84087816)(Day23.pt2)

  test("Day24"):
    if Day24.getEnv == Test then
      assertResult(5216)(Day24.pt1)
      assertResult(51)(Day24.pt2)

    if Day24.getEnv == Prod then
      assertResult(15165)(Day24.pt1)
      assertResult(4037)(Day24.pt2)

  test("Day25"):
    if Day25.getEnv == Test then
      assertResult(8)(Day25.pt1)

    if Day25.getEnv == Prod then
      assertResult(310)(Day25.pt1)
