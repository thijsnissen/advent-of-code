import scala.io.Source

object Day10 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: Vector[Point] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.map(Point.fromString)
			.toVector

	type Grid[A] = Vector[Vector[A]]

	object Grid:
		def fill[A](x:Int, y: Int)(a: => A): Grid[A] =
			Vector.fill(y, x)(a)

		def fromPoints(points: Vector[Point]): Grid[Char] =
			val xCor = math.abs(points.minBy(_.x).x)
			val yCor = math.abs(points.minBy(_.y).y)

			val (xArea, yArea) = Point.area(points)

			points.foldLeft(Grid.fill(xArea.toInt + 1, yArea.toInt + 1)('.')):
				case (g, p) if (0 to xArea.toInt).contains(p.x - xCor) && (0 to yArea.toInt).contains(p.y - yCor)
					=> g.updated(p.y - yCor, g(p.y - yCor).updated(p.x - xCor, '■'))
				case (g, _) => g

		extension [A](grid: Grid[A]) def asString: String =
			grid.map(row => row.map(_.toString).mkString("\n", " ", "")).mkString

	case class Point(x: Int, y: Int, vx: Int, vy: Int):
		def addVelocity: Point =
			copy(x = x + vx, y = y + vy)

	object Point:
		def fromString(s: String): Point =
			s match
				case s"position=<$x, $y> velocity=<$vx, $vy>" =>
					Point(x.trim.toInt, y.trim.toInt, vx.trim.toInt, vy.trim.toInt)

		def area(input: Vector[Point]): (Long, Long) =
			val xArea: Long = input.maxBy(_.x).x - input.minBy(_.x).x
			val yArea: Long = input.maxBy(_.y).y - input.minBy(_.y).y

			(xArea, yArea)

	def lookForMessageInTheSky(input: Vector[Point]): (Grid[Char], Int) =
		@annotation.tailrec
		def go(input: Vector[Point], prevArea: Long, time: Int): (Grid[Char], Int) =
			val newInput       = input.map(_.addVelocity)
			val (xArea, yArea) = Point.area(newInput)

			if xArea * yArea > prevArea then
				(Grid.fromPoints(input), time)
			else
				go(newInput, xArea * yArea, time + 1)

		go(input, Long.MaxValue, time = 0)

	import Grid._

	val startTime: Long =
		System.currentTimeMillis

	val (message, time): (Grid[Char], Int) = lookForMessageInTheSky(input)

	val answerPart1 = message.asString

	// test: HI [2ms], input: RPNNXFZR [130ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTime}ms]")

	val answerPart2 = time

	// test: 3 [2ms], input: 10946 [130ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTime}ms]")
