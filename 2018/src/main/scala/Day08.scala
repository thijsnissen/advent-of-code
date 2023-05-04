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
	// 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2

	def test(license: Vector[Int], acc: Vector[Int] = Vector.empty[Int]): (Vector[Int], Vector[Int]) =
		if license.size >= 2 then
			val children = license.head
			val metadata = license.drop(1).head
			val remaining = license.drop(2)

			if children == 0 then
				(remaining.drop(metadata), acc ++ remaining.take(metadata))
			else
				(0 until children).foldLeft((remaining, acc)):
					(x, _) => test(x._1, x._2)
		else
			(license, acc)

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = test(input) // test: 138 , input: 37439

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = ??? // test: , input:

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
