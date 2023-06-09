import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Day01"):
		assertResult(585)(actual = Day01.answerPart1)
		assertResult(83173)(actual = Day01.answerPart2)

	test("Day02"):
		assertResult(7105)(actual = Day02.answerPart1)
		assertResult("omlvgdokxfncvqyersasjziup")(actual = Day02.answerPart2)

	test("Day03"):
		assertResult(114946)(actual = Day03.answerPart1)
		assertResult(877)(actual = Day03.answerPart2)

	test("Day04"):
		assertResult(76357)(actual = Day04.answerPart1)
		assertResult(41668)(actual = Day04.answerPart2)

	test("Day05"):
		assertResult(9078)(actual = Day05.answerPart1)
		assertResult(5698)(actual = Day05.answerPart2)

	test("Day06"):
		assertResult(3989)(actual = Day06.answerPart1)
		assertResult(49715)(actual = Day06.answerPart2)

	test("Day07"):
		assertResult("IBJTUWGFKDNVEYAHOMPCQRLSZX")(actual = Day07.answerPart1)
		assertResult(1118)(actual = Day07.answerPart2)

	test("Day08"):
		assertResult(37439)(actual = Day08.answerPart1)
		assertResult(20815)(actual = Day08.answerPart2)

	test("Day09"):
		assertResult(388024)(actual = Day09.answerPart1)
		assertResult(3180929875L)(actual = Day09.answerPart2)

	test("Day10"):
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
				.stripMargin)(actual = Day10.answerPart1)
		assertResult(10946)(actual = Day10.answerPart2)

	test("Day11"):
		assertResult("243,27")(actual = Day11.answerPart1)
		assertResult("284,172,12")(actual = Day11.answerPart2)

	test("Day12"):
		assertResult(3915)(actual = Day12.answerPart1)
		assertResult(4900000001793L)(actual = Day12.answerPart2)

//import scala.io.Source
//
//object Day00 extends App:
//	val day: String =
//		this.getClass.getName.dropRight(1).toLowerCase
//
//	val input =
//		Source
//			.fromResource(s"$day-test.txt")
//			.getLines
//
//	val startTimePart1: Long =
//		System.currentTimeMillis
//
//	val answerPart1 = ???
//
//	// test: , input:
//	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")
//
//	val startTimePart2: Long =
//		System.currentTimeMillis
//
//	val answerPart2 = ???
//
//	// test: , input:
//	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
