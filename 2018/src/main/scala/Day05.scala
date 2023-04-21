import scala.io.Source

object Day05 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.flatMap(_.toVector)
			.toVector

	private val combinations: Vector[(Char, Char)] =
		(('a' to 'z').zip('A' to 'Z') ++ ('A' to 'Z').zip('a' to 'z')).toVector

	@annotation.tailrec
	private def removeReactingUnits(in: Vector[Char], acc: Vector[Char] = Vector.empty[Char], i: Int = 0): Vector[Char] =
		if in.size <= 1 && i == 0 then
			acc ++ in
		else if in.size <= 1 && i > 0 then
			removeReactingUnits(acc ++ in)
		else if combinations.contains((in(0), in(1))) then
			removeReactingUnits(in.drop(2), acc, i + 1)
		else
			removeReactingUnits(in.drop(1), acc ++ in.take(1), i)

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = removeReactingUnits(input).size // test: 10 [0ms], input: 9078 [8793ms]

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	private val shortestPolymer =
		('a' to 'z')
			.map(x => input.filterNot(_.toLower == x))
			.map(removeReactingUnits(_).size).min

	val answerPart2 = shortestPolymer // test: 4 [1ms] , input: 5698 [224917s]

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
