import scala.io.Source

object Day01 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Inventory =
		Source
			.fromResource(s"$day-test.txt")
			.getLines
			.foldLeft()(Inventory.parse)

	type Calories = List[Int]

	case class Inventory(lists: List[Calories])

	object Inventory:
		def parse(s: String): Inventory =
			if s.isEmpty then List.empty[Calories]
			else

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = ???

	// test: , input:
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = ???

	// test: , input:
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
