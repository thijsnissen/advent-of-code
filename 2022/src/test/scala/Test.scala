import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Day01"):
		if Day01.given_Mode == Day01.Mode.Test then
			assertResult(24000)(Day01.pt1) // [0ms]
			assertResult(45000)(Day01.pt2) // [0ms]

		if Day01.given_Mode == Day01.Mode.Prod then
			assertResult(68923)(Day01.pt1) // [0ms]
			assertResult(200044)(Day01.pt2) // [1ms]

// object Day00 extends AdventOfCode:
// 	given Mode = Mode.Test
//
// 	val myInput = input
//
// 	lazy val pt1 = ???
//
// 	lazy val pt2 = ???
//
// 	answer(1)(pt1)
//
// 	answer(2)(pt2)
