import scala.io.Source

object Day02 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: Vector[String] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.toVector

	def multipleLetterCount(input: Vector[String], n: Int): Int =
		input
			.map:
				l => l
					.groupBy(identity)
					.map(x => x._2.length)
					.toVector
			.count(l => l.contains(n))

	def findCommonLetters(input: Vector[String]): Option[String] =
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

	val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = multipleLetterCount(input, 2) * multipleLetterCount(input, 3)

	// test: 12 [1ms], input: 7105 [35ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	// test: fgij [0ms], input: omlvgdokxfncvqyersasjziup [29ms]
	val answerPart2 = findCommonLetters(input).getOrElse("Not Found")

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
