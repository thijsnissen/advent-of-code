package adventofcode
package aoc2018

import utilities.AdventOfCode
import utilities.Pos4D

object Day25 extends AdventOfCode:
	given Mode = Mode.Test

	val fixedPoints: Vector[Pos4D] =
		input
			.collect:
				case s"$x,$y,$z,$w" => Pos4D(x.toInt, y.toInt, z.toInt, w.toInt)
			.toVector

	lazy val pt1 =
		???

	lazy val pt2 =
		???

	answer(1)(pt1)

	answer(2)(pt2)
