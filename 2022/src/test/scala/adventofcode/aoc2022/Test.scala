package adventofcode
package aoc2022

import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Day01"):
		if Day01.given_Mode == Day01.Mode.Test then
			assertResult(24000)(Day01.pt1) // [0ms]
			assertResult(45000)(Day01.pt2) // [0ms]

		if Day01.given_Mode == Day01.Mode.Prod then
			assertResult(68923)(Day01.pt1) // [0ms]
			assertResult(200044)(Day01.pt2) // [1ms]

	test("Day02"):
		if Day02.given_Mode == Day02.Mode.Test then
			assertResult(15)(Day02.pt1) // [0ms]
			assertResult(12)(Day02.pt2) // [0ms]

		if Day02.given_Mode == Day02.Mode.Prod then
			assertResult(15422)(Day02.pt1) // [1ms]
			assertResult(15442)(Day02.pt2) // [1ms]
