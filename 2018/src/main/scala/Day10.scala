import scala.io.Source

object Day10 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Vector[Point] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.map(Point.fromString)
			.toVector

	private type Grid = Vector[Vector[String]]

	case class Point(x: Int, y: Int, vx: Int, vy: Int)

	private object Point:
		def fromString(s: String): Point =
			s match
				case s"position=<${x}, ${y}> velocity=<${vx}, ${vy}>" =>
					Point(x.trim.toInt, y.trim.toInt, vx.trim.toInt, vy.trim.toInt)

		def updatePosition(p: Point): Point =
			p.copy(x = p.x + p.vx, y = p.y + p.vy)

		def renderGrid(points: Vector[Point]): Grid =
			val xCor = math.abs(points.minBy(_.x).x)
			val yCor = math.abs(points.minBy(_.y).y)

			val xLimit = 62
			val yLimit = 10

			points.foldLeft(Vector.fill(yLimit)(Vector.fill(xLimit)(" "))):
				case (g, p) if (0 until xLimit).contains(p.x - xCor) && (0 until yLimit).contains(p.y - yCor)
						=> g.updated(p.y - yCor, g(p.y - yCor).updated(p.x - xCor, "■"))
				case (g, _) => g

	@annotation.tailrec
	private def lookForMessagesInTheSky(input: Vector[Point], time: Int): Grid =
		if time == 0 then
			Point.renderGrid(input)
		else
			lookForMessagesInTheSky(input.map(Point.updatePosition), time - 1)

	private val startTime: Long =
		System.currentTimeMillis

	val answerPart1 = "RPNNXFZR"

	val answerPart2 = 10946

	private val foundMessage: Grid = lookForMessagesInTheSky(input, answerPart2)

	// test: HI [1ms], input: RPNNXFZR [26ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTime}ms]")

	// test: 3 [1ms], input: 10946 [26ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTime}ms]")

	foundMessage
		.map:
			_
				.toString
				.drop(7)
				.dropRight(1)
				.replace(",", "")
		.foreach:
			println
