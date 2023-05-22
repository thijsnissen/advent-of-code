import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Day01"):
		assertResult(true)(actual = Day01.answerPart1)
		assertResult(true)(actual = Day01.answerPart2)

//import scala.io.Source
//
//object Day00 extends App:
//	private val day: String =
//		this.getClass.getName.dropRight(1).toLowerCase
//
//	private val input =
//		Source
//			.fromResource(s"$day-test.txt")
//			.getLines
//
//	private val startTimePart1: Long =
//		System.currentTimeMillis
//
//	val answerPart1 = ???
//
//	test: , input:
//	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")
//
//	private val startTimePart2: Long =
//		System.currentTimeMillis
//
//	val answerPart2 = ???
//
//	test: , input:
//	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
