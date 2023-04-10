import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:

	test("Day01") {
		assertResult(585)(actual = Day01.answerPart1)
		assertResult(83173)(actual = Day01.answerPart2)
	}

	test("Day02") {
		assertResult(7105)(actual = Day02.answerPart1)
		assertResult("omlvgdokxfncvqyersasjziup")(actual = Day02.answerPart2)
	}

	test("Day03") {
		assertResult(true)(actual = Day03.answerPart1)
		assertResult(true)(actual = Day03.answerPart2)
	}

//import scala.io.Source
//
//object Day01 extends App:
//	private val input: Vector[Int] =
//		Source
//			.fromResource("day01-test.txt")
//			.getLines
//			.map(_.toInt)
//			.toVector
//
//	private val startTimePart1: Long =
//		System.currentTimeMillis
//
//	val answerPart1 = ???// test: , input:
//
//	println(s"The answer to part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")
//
//	private val startTimePart2: Long =
//		System.currentTimeMillis
//
//	val answerPart2 = ??? // test: , input:
//
//	println(s"The answer to part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
