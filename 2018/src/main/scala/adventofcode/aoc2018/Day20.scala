package adventofcode
package aoc2018

import utilities.AdventOfCode
import utilities.Pos

object Day20 extends AdventOfCode:
	given Mode = Mode.Prod

	val regex: String =
		input.mkString

	enum Instructions:
		case Rooms(get: Vector[Char])
		case RoomsDetour(get: Vector[Char])
		case Branch(get: Vector[Leaf])
		case Leaf(get: Vector[Instructions])

	object Instructions:
		import utilities.Parser
		import utilities.Parser.*

		def rooms: Parser[Rooms] =
			for
				r <- oneOf("NESW").oneOrMore
			yield
				Rooms(r.toVector)

		def roomsDetour: Parser[RoomsDetour] =
			for
				_ <- char('(')
				r <- oneOf("NESW").oneOrMore
				_ <- string("|)")
			yield
				RoomsDetour(r.toVector)

		def branch: Parser[Branch] =
			for
				_ <- char('(')
				b <- (char('|').zeroOrMore ~ leaf).oneOrMore
				_ <- char(')')
			yield
				Branch(b.toVector)

		def leaf: Parser[Leaf] =
			for
				l <- (rooms | roomsDetour | branch).oneOrMore
			yield
				Leaf(l.toVector)

		def instructions: Parser[Leaf] =
			for
				_ <- char('^')
				r <- (rooms | roomsDetour | branch).oneOrMore
				_ <- char('$')
			yield
				Leaf(r.toVector)

	import Instructions.*

	def offset(p: Pos, c: Char): Pos =
		c match
			case 'N' => p + Pos(0, -1)
			case 'S' => p + Pos(0, 1)
			case 'E' => p + Pos(1, 0)
			case 'W' => p + Pos(-1, 0)

	def offsetRooms(rooms: Vector[Char], pos: Pos, dist: Int): Vector[(Pos, Int)] =
		rooms.foldLeft(Vector((pos, dist))):
			(a, c) =>
				val (p, d) = a.last
				a :+ (offset(p, c), d + 1)

	def instructionsToMap(instrutctions: Vector[Instructions]): Map[Pos, Int] =
		def loop(instr: Vector[Instructions], currPos: Pos, currDist: Int, acc: Vector[(Pos, Int)]): Vector[(Pos, Int)] =
			instr.headOption match
				case Some(Rooms(get)) =>
					val rooms =
						offsetRooms(get, currPos, currDist)

					val (maxPos, maxDist) =
						rooms.maxBy((_, d) => d)

					loop(instr.tail, maxPos, maxDist, acc ++ rooms.tail)
				case Some(RoomsDetour(get)) =>
					val rooms =
						offsetRooms(get.take(get.length / 2), currPos, currDist)

					loop(instr.tail, currPos, currDist, acc ++ rooms.tail)
				case Some(Branch(get)) =>
					loop(instr.tail ++ get, currPos, currDist, acc)
				case Some(Leaf(get)) =>
					val leaf =
						loop(get, currPos, currDist, Vector.empty[(Pos, Int)])

					loop(instr.tail, currPos, currDist, acc ++ leaf)
				case None =>
					acc

		loop(instrutctions, Pos.unit, 0, Vector.empty[(Pos, Int)])
			.groupMapReduce((p, _) => p)((_, i) => i)((a, b) => a min b)

	def parse(r: String): Vector[Instructions] =
		Instructions.instructions.run(r) match
			case Right(value) => value.get
			case Left(e)      => sys.error(e.asString)

	lazy val pt1 =
		val result =
			instructionsToMap(parse(regex))

		val (_, maxDistance) =
			result.maxBy((_, d) => d)

		maxDistance

	lazy val pt2 =
		val result =
			instructionsToMap(parse(regex))

		result.count((_, i) => i >= 1000)

	answer(1)(pt1)

	answer(2)(pt2)
