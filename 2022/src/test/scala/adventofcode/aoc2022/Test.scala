package adventofcode
package aoc2022

import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Day01"):
		if Day01.given_Mode == Day01.Mode.Test then
			assertResult(24000)(Day01.pt1) // [0ms]
			assertResult(45000)(Day01.pt2) // [0ms]

		if Day01.given_Mode == Day01.Mode.Prod then
			assertResult(68923)(Day01.pt1) // [1ms]
			assertResult(200044)(Day01.pt2) // [2ms]

	test("Day02"):
		if Day02.given_Mode == Day02.Mode.Test then
			assertResult(15)(Day02.pt1) // [0ms]
			assertResult(12)(Day02.pt2) // [0ms]

		if Day02.given_Mode == Day02.Mode.Prod then
			assertResult(15422)(Day02.pt1) // [1ms]
			assertResult(15442)(Day02.pt2) // [1ms]

	test("Day03"):
		if Day03.given_Mode == Day03.Mode.Test then
			assertResult(157)(Day03.pt1) // [0ms]
			assertResult(70)(Day03.pt2) // [0ms]

		if Day03.given_Mode == Day03.Mode.Prod then
			assertResult(8298)(Day03.pt1) // [1ms]
			assertResult(2708)(Day03.pt2) // [2ms]

	test("Day04"):
		if Day04.given_Mode == Day04.Mode.Test then
			assertResult(2)(Day04.pt1) // [0ms]
			assertResult(4)(Day04.pt2) // [0ms]

		if Day04.given_Mode == Day04.Mode.Prod then
			assertResult(424)(Day04.pt1) // [0ms]
			assertResult(804)(Day04.pt2) // [0ms]

	test("Day05"):
		if Day05.given_Mode == Day05.Mode.Test then
			assertResult("CMZ")(Day05.pt1) // [0ms]
			assertResult("MCD")(Day05.pt2) // [0ms]

		if Day05.given_Mode == Day05.Mode.Prod then
			assertResult("LBLVVTVLP")(Day05.pt1) // [1ms]
			assertResult("TPFFBDRJD")(Day05.pt2) // [1ms]

	test("Day06"):
		if Day06.given_Mode == Day06.Mode.Test then
			assertResult(7)(Day06.pt1) // [0ms]
			assertResult(19)(Day06.pt2) // [0ms]

		if Day06.given_Mode == Day06.Mode.Prod then
			assertResult(1850)(Day06.pt1) // [1ms]
			assertResult(2823)(Day06.pt2) // [2ms]

	test("Day07"):
		if Day07.given_Mode == Day07.Mode.Test then
			assertResult(95437)(Day07.pt1) // [0ms]
			assertResult(24933642)(Day07.pt2) // [0ms]

		if Day07.given_Mode == Day07.Mode.Prod then
			assertResult(1501149)(Day07.pt1) // [5ms]
			assertResult(10096985)(Day07.pt2) // [8ms]

	test("Day08"):
		if Day08.given_Mode == Day08.Mode.Test then
			assertResult(21)(Day08.pt1) // [1ms]
			assertResult(8)(Day08.pt2) // [0ms]

		if Day08.given_Mode == Day08.Mode.Prod then
			assertResult(1829)(Day08.pt1) // [57ms]
			assertResult(291840)(Day08.pt2) // [53ms]

	test("Day09"):
		if Day09.given_Mode == Day09.Mode.Test then
			assertResult(13)(Day09.pt1) // [1ms]
			assertResult(1)(Day09.pt2) // [0ms]

		if Day09.given_Mode == Day09.Mode.Prod then
			assertResult(5981)(Day09.pt1) // [10ms]
			assertResult(2352)(Day09.pt2) // [13ms]

	test("Day10"):
		if Day10.given_Mode == Day10.Mode.Test then
			assertResult(13140)(Day10.pt1) // [1ms]
			assertResult(
				"""
					|■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . . ■ ■ . .
					|■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ . . . ■ ■ ■ .
					|■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . . ■ ■ ■ ■ . . . .
					|■ ■ ■ ■ ■ . . . . . ■ ■ ■ ■ ■ . . . . . ■ ■ ■ ■ ■ . . . . . ■ ■ ■ ■ ■ . . . . .
					|■ ■ ■ ■ ■ ■ . . . . . . ■ ■ ■ ■ ■ ■ . . . . . . ■ ■ ■ ■ ■ ■ . . . . . . ■ ■ ■ ■
					|■ ■ ■ ■ ■ ■ ■ . . . . . . . ■ ■ ■ ■ ■ ■ ■ . . . . . . . ■ ■ ■ ■ ■ ■ ■ . . . . .
					|""".stripMargin)(Day10.pt2) // [0ms]

		if Day10.given_Mode == Day10.Mode.Prod then
			assertResult(14340)(Day10.pt1) // [1ms]
			assertResult(
				"""
					|■ ■ ■ . . . ■ ■ . . ■ ■ ■ . . . . ■ ■ . . ■ ■ . . ■ ■ ■ . . ■ . . ■ . ■ ■ ■ . .
					|■ . . ■ . ■ . . ■ . ■ . . ■ . . . . ■ . ■ . . ■ . ■ . . ■ . ■ . . ■ . ■ . . ■ .
					|■ . . ■ . ■ . . ■ . ■ . . ■ . . . . ■ . ■ . . . . ■ ■ ■ . . ■ ■ ■ ■ . ■ . . ■ .
					|■ ■ ■ . . ■ ■ ■ ■ . ■ ■ ■ . . . . . ■ . ■ . . . . ■ . . ■ . ■ . . ■ . ■ ■ ■ . .
					|■ . . . . ■ . . ■ . ■ . . . . ■ . . ■ . ■ . . ■ . ■ . . ■ . ■ . . ■ . ■ . . . .
					|■ . . . . ■ . . ■ . ■ . . . . . ■ ■ . . . ■ ■ . . ■ ■ ■ . . ■ . . ■ . ■ . . . .
					|""".stripMargin)(Day10.pt2) // [0ms]
