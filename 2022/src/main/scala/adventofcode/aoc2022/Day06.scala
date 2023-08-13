package adventofcode
package aoc2022

import utilities.AdventOfCode

object Day06 extends AdventOfCode:
	given Mode = Mode.Prod

	val dataStreamBuffer: String =
		input.mkString

	@annotation.tailrec
	def findMarkerLocation(stream: String, size: Int, acc: String = ""): Int =
		if (stream.head + acc.take(size - 1)).distinct.length == size then
			acc.length + 1
		else
			findMarkerLocation(stream.tail, size, stream.head + acc)

	lazy val pt1 =
		findMarkerLocation(dataStreamBuffer, 4)

	lazy val pt2 =
		findMarkerLocation(dataStreamBuffer, 14)

	answer(1)(pt1)

	answer(2)(pt2)
