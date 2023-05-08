import scala.io.Source

object Day06 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Vector[Coordinate] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.zipWithIndex
			.collect:
				case (s"$x, $y", i) => Coordinate(i, x.toInt, y.toInt)
			.toVector

	private case class Coordinate(id: Int, x: Int, y: Int):
		def distanceTo(tx: Int, ty: Int): Int =
			math.abs(x - tx) + math.abs(y - ty)

	private val startTimePart1: Long =
		System.currentTimeMillis

	private val maxX: Int = input.map(_.x).max + 1
	private val maxY: Int = input.map(_.y).max + 1

	private val grid1: Vector[Vector[Int]] =
		(0 until maxX).foldLeft(Vector.fill(maxY)(Vector.fill(maxX)(-1))):
			(state, x) => (0 until maxY).foldLeft(state):
				(state, y) =>
					val point = input.groupBy(_.distanceTo(x, y)).minBy(_._1)._2
					if point.size == 1 then
						state.updated(y, state(y).updated(x, point.head.id))
					else
						state

	private val infiniteAreas: Set[Int] =
		(grid1.head ++ grid1.last ++ grid1.flatMap(x => Vector(x.head, x.last))).toSet

	private val LargestNonInfiniteArea: Int =
		grid1
			.flatten
			.groupBy(identity)
			.map((id, area) => (id, area.size))
			.filterNot((id, _) => infiniteAreas.contains(id))
			.maxBy(_._2)
			._2

	val answerPart1 = LargestNonInfiniteArea

	// test: 17 [1ms], input: 3989 [999ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	private val maxDistance = 10000 // 32

	private val grid2: Vector[Vector[Int]] =
		(0 until maxX).foldLeft(Vector.fill(maxY)(Vector.fill(maxX)(-1))):
			(state, x) =>
				(0 until maxY).foldLeft(state):
					(state, y) =>
						if input.map(_.distanceTo(x, y)).sum < maxDistance then
							state.updated(y, state(y).updated(x, 0))
						else
							state

	private val sizeOfSafeRegion: Int =
		grid2
			.flatten
			.groupBy(identity)
			.map(x => (x._1, x._2.size))
			.find(_._1 == 0)
			.getOrElse((0, 0))
			._2

	val answerPart2 = sizeOfSafeRegion

	// test: 16 [0ms], input: 49715 [73ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
