package adventofcode
package aoc2018

import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Day01"):
		if Day01.given_Mode == Day01.Mode.Test then
			assertResult(4)(Day01.pt1) // [0ms]
			assertResult(10)(Day01.pt2) // [0ms]

		if Day01.given_Mode == Day01.Mode.Prod then
			assertResult(585)(Day01.pt1) // [9ms]
			assertResult(83173)(Day01.pt2) // [71ms]

	test("Day02"):
		if Day02.given_Mode == Day02.Mode.Test then
			assertResult(0)(Day02.pt1) // [4ms]
			assertResult("fgij")(Day02.pt2) // [3ms]

		if Day02.given_Mode == Day02.Mode.Prod then
			assertResult(7105)(Day02.pt1) // [16ms]
			assertResult("omlvgdokxfncvqyersasjziup")(Day02.pt2) // [68ms]

	test("Day03"):
		if Day03.given_Mode == Day03.Mode.Test then
			assertResult(4)(Day03.pt1) // [14ms]
			assertResult(3)(Day03.pt2) // [4ms]

		if Day03.given_Mode == Day03.Mode.Prod then
			assertResult(114946)(Day03.pt1) // [12ms]
			assertResult(877)(Day03.pt2) // [39ms]

	test("Day04"):
		if Day04.given_Mode == Day04.Mode.Test then
			assertResult(240)(Day04.pt1) // [1ms]
			assertResult(4455)(Day04.pt2) // [1ms]

		if Day04.given_Mode == Day04.Mode.Prod then
			assertResult(76357)(Day04.pt1) // [16ms]
			assertResult(41668)(Day04.pt2) // [4ms]

	test("Day05"):
		if Day05.given_Mode == Day05.Mode.Test then
			assertResult(10)(Day05.pt1) // [0ms]
			assertResult(4)(Day05.pt2) // [6ms]

		if Day05.given_Mode == Day05.Mode.Prod then
			assertResult(9078)(Day05.pt1) // [116ms]
			assertResult(5698)(Day05.pt2) // [1836ms]

	test("Day06"):
		if Day06.given_Mode == Day06.Mode.Test then
			assertResult(17)(Day06.pt1) // [8ms]
			assertResult(16)(Day06.pt2) // [0ms]

		if Day06.given_Mode == Day06.Mode.Prod then
			assertResult(3989)(Day06.pt1) // [1018ms]
			assertResult(49715)(Day06.pt2) // [85ms]

	test("Day07"):
		if Day07.given_Mode == Day07.Mode.Test then
			assertResult("CABDFE")(Day07.pt1) // [15ms]
			assertResult(253)(Day07.pt2) // [5ms]

		if Day07.given_Mode == Day07.Mode.Prod then
			assertResult("IBJTUWGFKDNVEYAHOMPCQRLSZX")(Day07.pt1) // [3ms]
			assertResult(1118)(Day07.pt2) // [13ms]

	test("Day08"):
		if Day08.given_Mode == Day08.Mode.Test then
			assertResult(138)(Day08.pt1) // [0ms]
			assertResult(66)(Day08.pt2) // [1ms]

		if Day08.given_Mode == Day08.Mode.Prod then
			assertResult(37439)(Day08.pt1) // [9ms]
			assertResult(20815)(Day08.pt2) // [3ms]

	test("Day09"):
		if Day09.given_Mode == Day09.Mode.Test then
			assertResult(32)(Day09.pt1) // [2ms]
			assertResult(22563)(Day09.pt2) // [8ms]

		if Day09.given_Mode == Day09.Mode.Prod then
			assertResult(388024)(Day09.pt1) // [43ms]
			assertResult(3180929875L)(Day09.pt2) // [1541ms]

	test("Day10"):
		if Day10.given_Mode == Day10.Mode.Test then
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
					.stripMargin)(Day10.pt1) // [3ms]
			assertResult(3)(Day10.pt2) // [0ms]

		if Day10.given_Mode == Day10.Mode.Prod then
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
					.stripMargin)(Day10.pt1) // [190ms]
			assertResult(10946)(Day10.pt2) // [145ms]

	test("Day11"):
		if Day11.given_Mode == Day11.Mode.Test then
			assertResult("33,45")(Day11.pt1) // [137ms]
			assertResult("90,269,16")(Day11.pt2) // [2340ms]

		if Day11.given_Mode == Day11.Mode.Prod then
			assertResult("243,27")(Day11.pt1) // [93ms]
			assertResult("284,172,12")(Day11.pt2) // [2432ms]

	test("Day12"):
		if Day12.given_Mode == Day12.Mode.Test then
			assertResult(325)(Day12.pt1) // [4ms]
			assertResult(999999999374L)(Day12.pt2) // [12ms]

		if Day12.given_Mode == Day12.Mode.Prod then
			assertResult(3915)(Day12.pt1) // [4ms]
			assertResult(4900000001793L)(Day12.pt2) // [14ms]

	test("Day13"):
		if Day13.given_Mode == Day13.Mode.Test then
			assertResult("7,3")(Day13.pt1) // [0ms]
			assertResult("7,3")(Day13.pt2) // [0ms]

		if Day13.given_Mode == Day13.Mode.Prod then
			assertResult("103,85")(Day13.pt1) // [4ms]
			assertResult("88,64")(Day13.pt2) // [20ms]

	test("Day14"):
		if Day14.given_Mode == Day14.Mode.Test then
			assertResult("0124515891")(Day14.pt1) // [1ms]
			assertResult(9)(Day14.pt2) // [0ms]

		if Day14.given_Mode == Day14.Mode.Prod then
			assertResult("7861362411")(Day14.pt1) // [110ms]
			assertResult(20203532)(Day14.pt2) // [2904ms]

	test("Day15"):
		if Day15.given_Mode == Day15.Mode.Test then
			assertResult(27730)(Day15.pt1) // [18ms]
			assertResult(4988)(Day15.pt2) // [17ms]

		if Day15.given_Mode == Day15.Mode.Prod then
			assertResult(181522)(Day15.pt1) // [896ms]
			assertResult(68324)(Day15.pt2) // [3763ms]

	test("Day16"):
		if Day16.given_Mode == Day16.Mode.Test then
			assertResult(1)(Day16.pt1) // [1ms]

		if Day16.given_Mode == Day16.Mode.Prod then
			assertResult(544)(Day16.pt1) // [7ms]
			assertResult(600)(Day16.pt2) // [4ms]

	test("Day17"):
		if Day17.given_Mode == Day17.Mode.Test then
			assertResult(57)(Day17.pt1) // [0ms]
			assertResult(29)(Day17.pt2) // [0ms]

		if Day17.given_Mode == Day17.Mode.Prod then
			assertResult(33242)(Day17.pt1) // [154ms]
			assertResult(27256)(Day17.pt2) // [24ms]

	test("Day18"):
		if Day18.given_Mode == Day18.Mode.Test then
			assertResult(1147)(Day18.pt1) // [5ms]
			assertResult(0)(Day18.pt2) // [9ms]

		if Day18.given_Mode == Day18.Mode.Prod then
			assertResult(519552)(Day18.pt1) // [44ms]
			assertResult(165376)(Day18.pt2) // [2621ms]
