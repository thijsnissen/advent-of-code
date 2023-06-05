import scala.io.Source

object Day11 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: Int =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.map(_.toInt)
			.next

	case class Window(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
		def size: Int =
			math.min(math.abs(xMin - xMax), math.abs(yMin - yMax)) + 1

	case class Cell(x: Int, y: Int):
		def powerLevel(gsn: Int): Int =
			val rackId: Int =
				x + 10

			val hundredsDigit: Int => Int =
				_ / 100 % 10

			hundredsDigit((rackId * y + gsn) * rackId) - 5

	type Grid[A] = Map[Cell, A]

	object Grid:
		def summedAreaTable(gsn: Int, window: Window): Grid[Int] =
			val grid =
				for
					x <- window.xMin to window.xMax
					y <- window.yMin to window.yMax
				yield
					Cell(x, y)

			grid.foldLeft(Map.empty[Cell, Int].withDefaultValue(0)):
				(g, c) =>
					g + (c -> (c.powerLevel(gsn) + g(Cell(c.x, c.y - 1)) + g(Cell(c.x - 1, c.y)) - g(Cell(c.x - 1, c.y - 1))))

		def largestTotalPowerForSquare(grid: Grid[Int], window: Window, squareSize: Int): (Cell, Int, Int) =
			val result =
				for
					x <- window.xMin to window.xMax
					y <- window.yMin to window.yMax
				yield
					(Cell(x - squareSize + 1, y - squareSize + 1), squareSize, grid(Cell(x, y)) - grid(Cell(x, y - squareSize)) - grid(Cell(x - squareSize, y)) + grid(Cell(x - squareSize, y - squareSize)))

			result.maxBy((_, _, totalPower) => totalPower)

		def largestTotalPower(grid: Grid[Int], window: Window): (Cell, Int, Int) =
			val result =
				for
					squareSize <- 1 to window.size
				yield
					largestTotalPowerForSquare(grid, window, squareSize)

			result.maxBy((_, _, totalPower) => totalPower)

	import Grid._

	val window = Window(1, 300, 1, 300)

	val startTimePart1: Long =
		System.currentTimeMillis

	val (c1, _, _) = largestTotalPowerForSquare(summedAreaTable(input, window), window, 3)

	val answerPart1 = s"${c1.x},${c1.y}"

	// test: 33,45 [100ms], input: 243,27 [130ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	val (c2, size, _) = largestTotalPower(summedAreaTable(input, window), window)

	val answerPart2 = s"${c2.x},${c2.y},$size"

	// test: 90,269,16 [4799ms], input: 284,172,12 [4596ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
