package adventofcode
package aoc2018

import utilities.AdventOfCode

object Day05 extends AdventOfCode:
	given Mode = Mode.Prod

	val myInput: String =
		input.mkString

	@annotation.tailrec
	def removeReactingUnits(input: String, acc: String = " "): String =
		if input.isEmpty then
			acc.reverse.trim
		else if input.head.toLower == acc.head.toLower && input.head != acc.head then
			removeReactingUnits(input.tail, acc.drop(1))
		else
			removeReactingUnits(input.tail, input.head + acc)

	def shortestPolymer =
		('a' to 'z')
			.map(x => myInput.filterNot(_.toLower == x))
			.map(removeReactingUnits(_).length).min

	lazy val pt1 =
		removeReactingUnits(myInput).length

	lazy val pt2 =
		shortestPolymer

	answer(1)(pt1)

	answer(2)(pt2)

// The implementation below takes about 8 seconds to complete part 1 and about 4 minutes to complete part 2
// Replaced with a better performing implementation, but kept here for reference
//
//	private val input =
//		Source
//			.fromResource(s"$day-input.txt")
//			.getLines
//			.flatMap(_.toVector)
//			.toVector
//
//	private val combinations: Vector[(Char, Char)] =
//		(('a' to 'z').zip('A' to 'Z') ++ ('A' to 'Z').zip('a' to 'z')).toVector
//
//	@annotation.tailrec
//	private def removeReactingUnits(in: Vector[Char], acc: Vector[Char] = Vector.empty[Char], i: Int = 0): Vector[Char] =
//		if in.size <= 1 && i == 0 then
//			acc ++ in
//		else if in.size <= 1 && i > 0 then
//			removeReactingUnits(acc ++ in)
//		else if combinations.contains((in(0), in(1))) then
//			removeReactingUnits(in.drop(2), acc, i + 1)
//		else
//			removeReactingUnits(in.drop(1), acc ++ in.take(1), i)
