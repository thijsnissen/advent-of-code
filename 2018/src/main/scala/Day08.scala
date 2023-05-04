import scala.io.Source

object Day08 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input =
		Source
			.fromResource(s"$day-test.txt")
			.getLines
			.mkString
			.split(" ")
			.map(_.toInt)
			.toVector

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = ??? // test: 138 , input: 37439

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = ??? // test: , input:

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
