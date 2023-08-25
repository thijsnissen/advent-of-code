package adventofcode
package aoc2022

import utilities.AdventOfCode
import utilities.Pos

object Day09 extends AdventOfCode:
	given Mode = Mode.Prod

	val instructions: Vector[Pos] =
		input
			.flatMap:
				case s"U $dist" => Vector.fill(dist.toInt)(Pos(0, 1))
				case s"R $dist" => Vector.fill(dist.toInt)(Pos(1, 0))
				case s"D $dist" => Vector.fill(dist.toInt)(Pos(0, -1))
				case s"L $dist" => Vector.fill(dist.toInt)(Pos(-1, 0))
			.toVector

	opaque type Rope = Vector[Pos]

	object Rope:
		def init(size: Int): Rope =
			Vector.fill(size)(Pos.zero)

		def empty: Rope =
			Vector.empty[Pos]

		def uniqueTailPositions(ropes: Vector[Rope]): Int =
			ropes
				.map(_.last)
				.distinct
				.length

		extension (self: Rope)
			def move(instruction: Pos): Rope =
				self
					.indices
					.foldLeft(Rope.empty):
						(acc, i) =>
							if i == 0 then acc :+ (self.head + instruction)
							else self(i).delta(acc.last) match
								case Pos(x, y) if x > 1 || y > 1 =>
									acc :+ (self(i) + acc.last.sign(self(i)))
								case _ => acc :+ self(i)

	import Rope.*

	lazy val pt1 =
		val result: Vector[Rope] =
			instructions
				.scanLeft(Rope.init(2)):
					(rope, instr) => rope.move(instr)

		uniqueTailPositions(result)

	lazy val pt2 =
		val result: Vector[Rope] =
			instructions
				.scanLeft(Rope.init(10)):
					(rope, instr) => rope.move(instr)

		uniqueTailPositions(result)

	answer(1)(pt1)

	answer(2)(pt2)
