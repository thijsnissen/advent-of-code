import scala.io.Source

object Day02 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Vector[String] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.toVector

	private val startTimePart1: Long =
		System.currentTimeMillis

	private def multipleLetterCount(input: Vector[String], n: Int): Int =
		input.map {
			l => l
				.groupBy(identity)
				.map(x => x._2.length)
				.toVector
		}.count(l => l.contains(n))

	private def findCommonLetters(input: Vector[String]): Option[String] =
		@annotation.tailrec
		def go(ids: Vector[Vector[(Char, Int)]]): Option[String] =
			if ids.nonEmpty then
				val index = ids.tail.indexWhere(id => id.diff(ids.head).size == 1)

				if index >= 0 then
					Some(ids.tail(index).intersect(ids.head).map(_._1).mkString)
				else
					go(ids.tail)
			else
				None

		go(input.map(_.zipWithIndex.toVector))

	val answerPart1 = multipleLetterCount(input, 2) * multipleLetterCount(input, 3) // test: 12 [1ms], input: 7105

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = findCommonLetters(input).getOrElse("Not Found") // test: fgij [0ms], input: omlvgdokxfncvqyersasjziup [29ms]

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
