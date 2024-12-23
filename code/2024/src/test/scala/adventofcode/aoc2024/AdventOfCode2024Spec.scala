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

  test("Day12"):
    if Day12.getEnv == Test then
      assertResult(1930)(Day12.pt1)
      assertResult(1206)(Day12.pt2)

    if Day12.getEnv == Prod then
      assertResult(1400386)(Day12.pt1)
      assertResult(851994)(Day12.pt2)

  test("Day13"):
    if Day13.getEnv == Test then
      assertResult(480)(Day13.pt1)
      assertResult(875318608908L)(Day13.pt2)

    if Day13.getEnv == Prod then
      assertResult(37297)(Day13.pt1)
      assertResult(83197086729371L)(Day13.pt2)

  test("Day14"):
    if Day14.getEnv == Test then
      assertResult(12)(Day14.pt1)
      assertResult(5)(Day14.pt2)

    if Day14.getEnv == Prod then
      assertResult(220971520)(Day14.pt1)
      assertResult(6355)(Day14.pt2)

  test("Day15"):
    if Day15.getEnv == Test then
      assertResult(10092)(Day15.pt1)
      assertResult(9021)(Day15.pt2)

    if Day15.getEnv == Prod then
      assertResult(1383666)(Day15.pt1)
      assertResult(1412866)(Day15.pt2)

  test("Day16"):
    if Day16.getEnv == Test then
      assertResult(7036)(Day16.pt1)
      assertResult(45)(Day16.pt2)

    if Day16.getEnv == Prod then
      assertResult(105508)(Day16.pt1)
      assertResult(548)(Day16.pt2)

  test("Day17"):
    if Day17.getEnv == Test then
      assertResult("5,7,3,0")(Day17.pt1)
      assertResult(117440)(Day17.pt2)

    if Day17.getEnv == Prod then
      assertResult("7,6,1,5,3,1,4,2,6")(Day17.pt1)
      assertResult(164541017976509L)(Day17.pt2)

  test("Day18"):
    if Day18.getEnv == Test then
      assertResult(22)(Day18.pt1)
      assertResult("6,1")(Day18.pt2)

    if Day18.getEnv == Prod then
      assertResult(308)(Day18.pt1)
      assertResult("46,28")(Day18.pt2)

  test("Day19"):
    if Day19.getEnv == Test then
      assertResult(6)(Day19.pt1)
      assertResult(16)(Day19.pt2)

    if Day19.getEnv == Prod then
      assertResult(350)(Day19.pt1)
      assertResult(769668867512623L)(Day19.pt2)

  test("Day20"):
    if Day20.getEnv == Test then
      assertResult(5)(Day20.pt1)
      assertResult(285)(Day20.pt2)

    if Day20.getEnv == Prod then
      assertResult(1384)(Day20.pt1)
      assertResult(1008542)(Day20.pt2)

  test("Day21"):
    if Day21.getEnv == Test then
      assertResult(126384)(Day21.pt1)
      assertResult(154115708116294L)(Day21.pt2)

    if Day21.getEnv == Prod then
      assertResult(248108)(Day21.pt1)
      assertResult(303836969158972L)(Day21.pt2)

  test("Day22"):
    if Day22.getEnv == Test then
      assertResult(37990510)(Day22.pt1)
      assertResult(23)(Day22.pt2)

    if Day22.getEnv == Prod then
      assertResult(19822877190L)(Day22.pt1)
      assertResult(2277)(Day22.pt2)

  test("Day23"):
    if Day23.getEnv == Test then
      assertResult(7)(Day23.pt1)
      assertResult("co,de,ka,ta")(Day23.pt2)

    if Day23.getEnv == Prod then
      assertResult(1064)(Day23.pt1)
      assertResult("aq,cc,ea,gc,jo,od,pa,rg,rv,ub,ul,vr,yy")(Day23.pt2)
