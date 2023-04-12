import scala.io.Source

object Day01 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Vector[Long] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.map(_.toLong)
			.toVector

	private def firstSumSeenTwice(input: Vector[Long]): Long =
		@annotation.tailrec
		def go(in: Vector[Long], seen: Set[Long] = Set(0), acc: Long = 0): Long =
			if in.isEmpty then go(input, seen, acc)
			else if seen.contains(acc + in.head) then acc + in.head
			else go(in.tail, seen + (acc + in.head), acc + in.head)

		go(input)

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = input.sum // test: 4 [0ms], input: 585 [0ms]

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = firstSumSeenTwice(input) // test: 10 [0ms], input: 83173 [46ms]

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
