package adventofcode
package aoc2018

import utilities.AdventOfCode
import utilities.Pos

object Day20 extends AdventOfCode:
	given Mode = Mode.Prod

	val route =
		input
			.mkString
			.map(Route.fromChar)

		object Route:
			def fromChar(c: Char): Pos =
				c match
					case 'N' => ???
					case 'E' => ???
					case 'S' => ???
					case 'W' => ???

	pprint.log(route)

	lazy val pt1 =
		???

	lazy val pt2 =
		???

	answer(1)(pt1)

	answer(2)(pt2)
