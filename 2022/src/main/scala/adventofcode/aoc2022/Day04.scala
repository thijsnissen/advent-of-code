package adventofcode
package aoc2022

import utilities.AdventOfCode

object Day04 extends AdventOfCode:
	given Mode = Mode.Prod

	val elfPairs: Vector[ElfPair] =
		input
			.collect:
				case s"$a-$b,$c-$d" => ElfPair(a.toInt, b.toInt, c.toInt, d.toInt)
			.toVector

	case class ElfPair(aFrom: Int, aTo: Int, bFrom: Int, bTo: Int):
		def overlap: Boolean =
			(aFrom <= bTo && aFrom >= bFrom) || (bFrom <= aTo && bFrom >= aFrom)

		def fullOverlap: Boolean =
			(aFrom <= bFrom && aTo >= bTo) || (bFrom <= aFrom && bTo >= aTo)

	lazy val pt1 =
		elfPairs
			.count(_.fullOverlap)

	lazy val pt2 =
		elfPairs
			.count(_.overlap)

	answer(1)(pt1)

	answer(2)(pt2)
