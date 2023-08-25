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

	// Turn each direction/character into a Pos(x, y) and a corresponding
	// total travel distance so far.
	def offsetRooms(rooms: Vector[Char], pos: Pos, dist: Int): Vector[(Pos, Int)] =
		rooms.foldLeft(Vector((pos, dist))):
			(a, c) =>
				val (p, d) = a.last
				a :+ (offset(p, c), d + 1)

	def instructionsToFloorplan(instructions: Vector[Instructions]): Map[Pos, Int] =
		def loop(instr: Vector[Instructions], currPos: Pos, currDist: Int, acc: Vector[(Pos, Int)]): Vector[(Pos, Int)] =
			instr.headOption match
				// Once we encounter a seqence of rooms, the deepest level of the Instructions
				// datastructure, we get for each room it's Pos and the distance travelled up to that room.
				// We add them to the accumulator and set the currPos and currDist to the furthest
				// we've gone so far. Continue with the next instruction (instr.tail).
				case Some(Rooms(get)) =>
					val rooms =
						offsetRooms(get, currPos, currDist)

					val (maxPos, maxDist) =
						rooms.maxBy((_, d) => d)

					loop(instr.tail, maxPos, maxDist, acc ++ rooms.tail)

				// A sequence of rooms that lead to a dead end and are then travelled again
				// backwards (SSEE WWNN). Because of that we only have to index half of the
				// route. Since a RoomsDetour leads back to the point where it started we
				// continue to the next instruction (instr.tail) with the currPos and
				// currDist unchanged.
				case Some(RoomsDetour(get)) =>
					val rooms =
						offsetRooms(get.take(get.length / 2), currPos, currDist)

					loop(instr.tail, currPos, currDist, acc ++ rooms.tail)

				// A Branch consists of multiple leafs. It is basically being able to turn
				// left and right on an intersection. Each direction consists of a set of
				// instructions (Leaf) that have to be explored separately from the currPos
				// and currDist. All we need to do is add them to the queue for further
				// processing from the currPos and currDist.
				case Some(Branch(get)) =>
					loop(instr.tail ++ get, currPos, currDist, acc)

				// A leaf is a wrapper around a series of instructions that occured from
				// a branch (basically being able to turn left and right on an intersection).
				// Once a leaf (i.e. pick a direction) is fully explored, it's result is
				// added to the accumulator and we continue to the next instruction
				// (instr.tail) from the point of the intersection (currPos and currDist).
				case Some(Leaf(get)) =>
					val leaf =
						loop(get, currPos, currDist, Vector.empty[(Pos, Int)])

					loop(instr.tail, currPos, currDist, acc ++ leaf)

				// When all the instructions are processed we return the accumulator.
				case None =>
					acc

		// In the full puzzle input some rooms are visited multiple times. This
		// apparently happens not only in the roomsDetour (because there we account
		// for it) but also in the main route. With groupMapReduce we pick the shortest
		// distance for every Pos and remove the rest.
		loop(instructions, Pos.zero, 0, Vector.empty[(Pos, Int)])
			.groupMapReduce((p, _) => p)((_, i) => i)((a, b) => a min b)

	def parse(r: String): Vector[Instructions] =
		Instructions.instructions.run(r) match
			case Right(value) => value.get
			case Left(e)      => sys.error(e.asString)

	lazy val pt1 =
		val result =
			instructionsToFloorplan(parse(regex))

		val (_, maxDistance) =
			result.maxBy((_, d) => d)

		maxDistance

	lazy val pt2 =
		instructionsToFloorplan(parse(regex))
			.count((_, i) => i >= 1000)

	answer(1)(pt1)

	answer(2)(pt2)
