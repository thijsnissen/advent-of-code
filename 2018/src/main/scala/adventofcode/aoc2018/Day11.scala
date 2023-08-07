package adventofcode
package aoc2018

import utilities.AdventOfCode

object Day11 extends AdventOfCode:
	given Mode = Mode.Prod

	val myInput: Int =
		input
			.map(_.toInt)
			.next

	case class Result(cell: Cell, squareSize: Int, totalPower: Int)

	case class Window(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
		def maxSquareSize: Int =
			math.min(math.abs(xMin - xMax), math.abs(yMin - yMax)) + 1

	case class Cell(x: Int, y: Int):
		val powerLevel: Int => Int =
			gsn =>
				val rackId: Int =
					x + 10

				val hundredsDigit: Int => Int =
					_ / 100 % 10

				hundredsDigit((rackId * y + gsn) * rackId) - 5

	type Grid[A] = Map[Cell, A]

	object Grid:
		def empty: Grid[Int] =
			Map.empty[Cell, Int].withDefaultValue(0)

		// ref: https://en.wikipedia.org/wiki/Summed-area_table
		def summedAreaTable(gsn: Int, window: Window): Grid[Int] =
			val grid =
				for
					x <- window.xMin to window.xMax
					y <- window.yMin to window.yMax
				yield
					Cell(x, y)

			grid.foldLeft(Grid.empty):
				(acc, c) =>
					acc + (c -> (
						c.powerLevel(gsn) +
						acc(Cell(c.x, c.y - 1)) +
						acc(Cell(c.x - 1, c.y)) -
						acc(Cell(c.x - 1, c.y - 1))
					))

		def largestTotalPowerForSquare(grid: Grid[Int], window: Window, squareSize: Int): Result =
			val result =
				for
					x <- window.xMin to window.xMax
					y <- window.yMin to window.yMax
					if x >= squareSize && y >= squareSize
				yield
					Result(
						Cell(x - squareSize + 1, y - squareSize + 1),
						squareSize,
						grid(Cell(x, y)) -
							grid(Cell(x, y - squareSize)) -
							grid(Cell(x - squareSize, y)) +
							grid(Cell(x - squareSize, y - squareSize))
					)

			result.maxBy(_.totalPower)

		def largestTotalPower(grid: Grid[Int], window: Window): Result =
			val result =
				for
					squareSize <- 1 to window.maxSquareSize
				yield
					largestTotalPowerForSquare(grid, window, squareSize)

			result.maxBy(_.totalPower)

	import Grid.*

	val window = Window(1, 300, 1, 300)

	lazy val pt1 =
		val r1 =
			largestTotalPowerForSquare(summedAreaTable(myInput, window), window, 3)

		s"${r1.cell.x},${r1.cell.y}"

	lazy val pt2 =
		val r2 = largestTotalPower(summedAreaTable(myInput, window), window)

		s"${r2.cell.x},${r2.cell.y},${r2.squareSize}"

	answer(1)(pt1)

	answer(2)(pt2)
