import scala.io.Source

object Day05 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: String =
		Source
			.fromResource(s"day05-input.txt")
			.getLines
			.mkString

	@annotation.tailrec
	def removeReactingUnits(input: String, acc: String = " "): String =
		if input.isEmpty then
			acc.reverse.trim
		else if input.head.toLower == acc.head.toLower && input.head != acc.head then
			removeReactingUnits(input.tail, acc.drop(1))
		else
			removeReactingUnits(input.tail, input.head + acc)

	val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = removeReactingUnits(input).length

	// test: 10 [0ms], input: 9078 [98ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	val shortestPolymer =
		('a' to 'z')
			.map(x => input.filterNot(_.toLower == x))
			.map(removeReactingUnits(_).length).min

	val answerPart2 = shortestPolymer

	// test: 4 [1ms] , input: 5698 [1791ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")

// The implementation below takes about 8 seconds to complete part 1 and about 4 minutes to complete part 2
// Replaced with a better performing implementation, but kept here for reference
//
//	private val input =
//		Source
//			.fromResource(s"$day-input.txt")
//			.getLines
//			.flatMap(_.toVector)
//			.toVector
//
//	private val combinations: Vector[(Char, Char)] =
//		(('a' to 'z').zip('A' to 'Z') ++ ('A' to 'Z').zip('a' to 'z')).toVector
//
//	@annotation.tailrec
//	private def removeReactingUnits(in: Vector[Char], acc: Vector[Char] = Vector.empty[Char], i: Int = 0): Vector[Char] =
//		if in.size <= 1 && i == 0 then
//			acc ++ in
//		else if in.size <= 1 && i > 0 then
//			removeReactingUnits(acc ++ in)
//		else if combinations.contains((in(0), in(1))) then
//			removeReactingUnits(in.drop(2), acc, i + 1)
//		else
//			removeReactingUnits(in.drop(1), acc ++ in.take(1), i)
