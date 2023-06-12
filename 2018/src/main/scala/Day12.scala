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
				.filter((c, _) => c == '#')
				.map((_, i) => i - generation)
				.sum

	object Mutation:
		def fromInput(input: Vector[String]): Mutation =
			Mutation(input.head, input.tail, 0)

	val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 =
		Mutation
			.fromInput(input)
			.mutate(20)
			.sumOfAllPotsWithPlants

	// test: 325 [2ms], input: 3915 [3ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	// After a while the sum of every next generation increases with a fixed amount
	// Here I pick a generation to start from of which I know the incremented value is constant.
	// TODO: https://en.wikipedia.org/wiki/Cycle_detection
	val generation: Long = 100

	val sum: Long =
		Mutation
			.fromInput(input)
			.mutate(generation)
			.sumOfAllPotsWithPlants

	val interval: Long =
		Mutation
			.fromInput(input)
			.mutate(generation + 1)
			.sumOfAllPotsWithPlants
		- sum

	val answerPart2 = sum + (50000000000L - generation) * interval

	// test: 999999999374 [11ms], input: 4900000001793 [15ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
