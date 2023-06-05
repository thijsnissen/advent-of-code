import scala.io.Source

object Day03 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: Vector[Claim] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.collect:
				case s"#${id} @ ${left},${top}: ${width}x${height}" =>
					Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
			.toVector

	case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int):
		val xStart: Int = left
		val xEnd: Int = left + width - 1
		val yStart: Int = top
		val yEnd: Int = top + height - 1

		def getSquares: Set[Square] =
			(yStart to yEnd)
				.flatMap:
					y =>
						(xStart to xEnd).map:
							x => Square(x, y, id)
				.toSet

	case class Square(x: Int, y: Int, id: Int)

	def findClaimIDsWithOverlap(input: Vector[Claim]): Set[Int] =
		input
			.flatMap(_.getSquares)
			.groupBy:
				case Square(x, y, _) => (x, y)
			.filter((_, s) => s.size > 1)
			.flatMap(c => c._2)
			.map:
				case Square(_, _, id) => id
			.toSet

	val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 =
		input
			.flatMap(_.getSquares)
			.groupBy:
				case Square(x, y, _) => (x, y)
			.count((_, s) => s.size > 1)

	// test: 4 [0ms], input: 114946 [350ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 =
		input
			.map(_.id)
			.diff(findClaimIDsWithOverlap(input).toVector).head

	// test: 3 [1ms], input: 877 [475ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
