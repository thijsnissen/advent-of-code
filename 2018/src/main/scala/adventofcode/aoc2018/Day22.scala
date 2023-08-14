package adventofcode
package aoc2018

import utilities.AdventOfCode
import utilities.Pos
import utilities.Box

object Day22 extends AdventOfCode:
	given Mode = Mode.Test

	val depth: Int =
			input
				.collect:
					case s"depth: $d" => d.toInt
				.next

	val target: Pos =
		input
			.collect:
				case s"target: $x,$y" => Pos(x.toInt, y.toInt)
			.next

	val mouthOfCave =
		Pos.unit

	enum RegionType:
		case Rocky
		case Wet
		case Narrow

	object RegionType:
		def fromErosionLevel(el: Int): RegionType =
			RegionType.fromOrdinal(el % 3)

	import RegionType.*

	case class Regions(mouthOfCave: Pos, target: Pos, depth: Int, regions: Map[Pos, RegionType]):
		def riskLevel: Int =
			regions
				.map:
					case (_, Rocky)  => 0
					case (_, Wet)    => 1
					case (_, Narrow) => 2
				.sum

	object Regions:
		def unit(mouthOfCave: Pos, target: Pos, depth: Int): Regions =
			val regions: Map[Pos, RegionType] =
				Box(mouthOfCave, target)
					.iterator
					.foldLeft(Map.empty[Pos, Int]):
						(acc, p) => acc.get(p) match
							case Some(el) =>
								acc + (p -> el)
							case None =>
								val newAcc = erosionLevel(p, acc)

								newAcc + (p -> newAcc(p))
					.map((p, el) => p -> fromErosionLevel(el))

			Regions(mouthOfCave, target, depth, regions)

		def erosionLevel(pos: Pos, acc: Map[Pos, Int]): Map[Pos, Int] =
			pos match
				case p if p == mouthOfCave || p == target => acc + (p -> (depth % 20183))
				case Pos(x, 0) => acc + (Pos(x, 0) -> ((x * 16807 + depth) % 20183))
				case Pos(0, y) => acc + (Pos(0, y) -> ((y * 48271 + depth) % 20183))
				case Pos(x, y) =>
					val levelsA: Map[Pos, Int] = acc.get(Pos(x - 1, y)) match
						case Some(_) => acc
						case None => erosionLevel(Pos(x - 1, y), acc)

					val levelsB: Map[Pos, Int] = acc.get(Pos(x, y - 1)) match
						case Some(_) => acc
						case None => erosionLevel(Pos(x, y - 1), acc)

					val newLevels = levelsA ++ levelsB

					newLevels + (Pos(x, y) -> (newLevels(Pos(x - 1, y)) * newLevels(Pos(x, y - 1)) + depth) % 20183)

	lazy val pt1 =
		Regions
			.unit(mouthOfCave, target, depth)
			.riskLevel

	lazy val pt2 =
		???

	answer(1)(pt1)

	answer(2)(pt2)
