object Day01 extends AdventOfCode:
	given Mode = Mode.Prod

	val calories: Vector[Vector[Int]] =
		input
			.foldLeft(Vector(Vector.empty[Int])):
				case (acc, "") => Vector.empty[Int] +: acc
				case (acc, cal) => acc.updated(0, cal.toInt +: acc(0))

	lazy val pt1 =
		calories
			.map(_.sum)
			.max

	lazy val pt2 =
		calories
			.map(_.sum)
			.sorted
			.takeRight(3)
			.sum

	answer(1)(pt1)

	answer(2)(pt2)
