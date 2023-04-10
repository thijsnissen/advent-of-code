import scala.io.Source

object Day03 extends App:
	private val input: Vector[Claim] =
		Source
			.fromResource("day03-test.txt")
			.getLines
			.collect {
				case s"#${id} @ ${left},${top}: ${width}x${height}" =>
					Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
			}
			.toVector

	private final case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int):
		val xStart: Int = left
		val xEnd: Int   = left + width - 1
		val yStart: Int = top
		val yEnd: Int   = top + height - 1

	private val xMax: Int = input.map(_.xEnd).max + 1
	private val yMax: Int = input.map(_.yEnd).max + 1

	private val grid: Vector[Vector[String]] = input.foldLeft(Vector.fill(yMax)(Vector.fill(xMax)("."))) {
		(state, claim) =>
			for
				y <- (claim.yStart to claim.yEnd).toVector
				x <- (claim.xStart to claim.xEnd).toVector
			yield
				val filler = if state(y)(x) != "." then "#" else claim.id.toString

				state.updated(y, state(y).updated(x, filler))
		}

	grid.foreach(println)

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = grid.flatten.count(_ == "#") // test: , input:

	println(s"The answer to part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = ??? // test: , input:

	println(s"The answer to part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
