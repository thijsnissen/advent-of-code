object Day14 extends AdventOfCode:
	given Mode = Mode.Prod

	val recepiesCount: Int =
		input
			.map(_.toInt)
			.next

	object Recepies:
		extension (self: Int)
			def fromInt: Vector[Int] =
				self
					.toString
					.map(_.asDigit)
					.toVector

	import Recepies.fromInt

	@annotation.tailrec
	def loop(scoreboard: Vector[Int], elves: Vector[Int])(f: Vector[Int] => Boolean): Vector[Int] =
		if f(scoreboard) then
			scoreboard
		else
			val newRecepies =
				elves.map(scoreboard)

			val newScoreboard =
				scoreboard ++ newRecepies.sum.fromInt

			val newElves =
				elves.zip(newRecepies).map:
					(i, s) => (i + 1 + s) % newScoreboard.length

			loop(newScoreboard, newElves)(f)

	lazy val pt1 =
		val result =
			loop(Vector(3, 7), Vector(0, 1)):
				s => s.length >= recepiesCount + 10

		result
			.slice(recepiesCount, recepiesCount + 10)
			.mkString

	lazy val pt2 =
		val recepiesCountSlice =
			recepiesCount.fromInt

		val elves  = Vector(0, 1)
		val offset = elves.length - 1

		val result =
			loop(Vector(3, 7), elves):
				s => s.takeRight(recepiesCountSlice.length + offset).containsSlice(recepiesCountSlice)

		result.indexOfSlice(recepiesCountSlice)

	answer(1)(pt1)

	answer(2)(pt2)
