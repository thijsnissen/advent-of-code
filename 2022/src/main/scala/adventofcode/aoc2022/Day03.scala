package adventofcode
package aoc2022

import utilities.AdventOfCode

object Day03 extends AdventOfCode:
	given Mode = Mode.Prod

	val rucksacks: Vector[String] =
		input.toVector

	val priorities: Vector[Char] =
		(('a' to 'z') ++ ('A' to 'Z')).toVector

	def itemInBothCompartments(s: String): Char =
		val l = s.take(s.length / 2)
		val r = s.drop(s.length / 2)

		l.intersect(r).charAt(0)

	def commonItem(elves: Vector[String]): Char =
		val Vector(a, b, c) = elves

		a.intersect(b).intersect(c).charAt(0)

	lazy val pt1 =
		rucksacks
			.map:
				s => priorities
					.indexOf(itemInBothCompartments(s)) + 1
			.sum

	lazy val pt2 =
		rucksacks
			.grouped(3)
			.map:
				g => priorities
					.indexOf(commonItem(g)) + 1
			.sum

	answer(1)(pt1)

	answer(2)(pt2)
