import scala.io.Source

object Day12 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: Vector[String] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.collect:
				case s"initial state: $state" => state
				case s"$pattern => #" => pattern
			.toVector

	case class Mutation(state: String, rules: Vector[String], generation: Long):
		@annotation.tailrec
		final def mutate(generations: Long): Mutation =
			if generations <= 0 then
				this
			else
				val mutatedState =
					("..." + state + "...").sliding(5).foldLeft(""):
						case (acc, s) if rules.contains(s) => acc + "#"
						case (acc, _) => acc + '.'

				copy(state = mutatedState, generation = generation + 1).mutate(generations - 1)

		def sumOfAllPotsWithPlants: Long =
			state
				.zipWithIndex
				.collect:
					case ('#', i) => i - generation
				.sum

	object Mutation:
		def fromInput(input: Vector[String]): Mutation =
			Mutation(input.head, input.tail, 0)

		def empty: Mutation =
			Mutation("", Vector.empty[String], 0)

	val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 =
		Mutation
			.fromInput(input)
			.mutate(20)
			.sumOfAllPotsWithPlants

	// test: 325 [1ms], input: 3915 [2ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	val comparisonIterator =
		Iterator
			.iterate((Mutation.fromInput(input), Mutation.empty)):
				(curr, _) => (curr.mutate(1), curr)

	val (generation, sum, interval) =
		comparisonIterator
			.dropWhile:
				(curr, prev) =>
					curr.state.dropWhile(_ == '.') != prev.state.dropWhile(_ == '.')
			.map:
				(curr, prev) =>
					(curr.generation, curr.sumOfAllPotsWithPlants, curr.sumOfAllPotsWithPlants - prev.sumOfAllPotsWithPlants)
			.next

	val answerPart2 = sum + (50000000000L - generation) * interval

	// test: 999999999374 [5ms], input: 4900000001793 [8ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
