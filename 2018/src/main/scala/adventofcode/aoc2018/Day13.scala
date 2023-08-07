package adventofcode
package aoc2018

import utilities.AdventOfCode
import utilities.Pos

object Day13 extends AdventOfCode:
	given Mode = Mode.Prod

	val cartPieces  = Set('^', 'v', '<', '>')
	val trackPieces = Set('|', '-', '\\', '/', '+')

	val tracks: Map[Pos, Char] =
		def cartToTrack(c: Char): Char =
			c match
				case '^' | 'v' => '|'
				case '<' | '>' => '-'
				case _         => c

		val tracks =
			for
				(l, y) <- input.zipWithIndex
				(c, x) <- l.zipWithIndex

				if cartPieces.contains(c) | trackPieces.contains(c)
			yield
				(Pos(x, y), cartToTrack(c))

		tracks.toMap

	val carts: Vector[Cart] =
		def cartToCart(c: Char, x: Int, y: Int): Cart =
			c match
				case '^' => Cart(Pos(x, y), Pos(0, -1), 0)
				case 'v' => Cart(Pos(x, y), Pos(0, 1), 0)
				case '<' => Cart(Pos(x, y), Pos(-1, 0), 0)
				case '>' => Cart(Pos(x, y), Pos(1, 0), 0)

		val carts =
			for
				(l, y) <- input.zipWithIndex
				(c, x) <- l.zipWithIndex

				if cartPieces.contains(c)
			yield
				cartToCart(c, x, y)

		carts.toVector

	case class Cart(loc: Pos, vector: Pos, intersectionCount: Int):
		def turnLeft: Cart =
			copy(vector = Pos(vector.y, -vector.x))

		def turnRight: Cart =
			copy(vector = Pos(-vector.y, vector.x))

		def turnBackward: Cart =
			copy(vector = Pos(vector.y, vector.x))

		def turnForward: Cart =
			copy(vector = Pos(-vector.y, -vector.x))

		def intersection: Cart =
			intersectionCount % 3 match
				case 0 => turnLeft.copy(intersectionCount = intersectionCount + 1)
				case 1 => copy(intersectionCount = intersectionCount + 1)
				case 2 => turnRight.copy(intersectionCount = intersectionCount + 1)

		def move: Cart =
			copy(loc = loc + vector)

		def turn(track: Char): Cart =
			track match
				case '/'  => turnForward
				case '\\' => turnBackward
				case '+'  => intersection
				case _    => this

	given Ordering[Pos] =
		Ordering.fromLessThan:
			(a, b) => a.y < b.y || (a.y == b.y && a.x < b.x)

	def findCollisionPos(carts: Vector[Cart], cart: Cart): Option[Pos] =
		carts.find(_.loc == cart.loc).map(_.loc)

	def findFirstCollisionPos(carts: Vector[Cart], tracks: Map[Pos, Char])(using Ordering[Pos]): Pos =
		@annotation.tailrec
		def tick(toMove: Vector[Cart], moved: Vector[Cart]): Pos =
			if toMove.isEmpty then
				tick(moved.sortBy(_.loc), Vector.empty[Cart])
			else
				val movedCart      = toMove.head.move
				val remainingCarts = toMove.tail ++ moved

				findCollisionPos(remainingCarts, movedCart) match
					case Some(pos) => pos
					case None      => tick(toMove.tail, moved :+ movedCart.turn(tracks(movedCart.loc)))

		tick(carts.sortBy(_.loc), Vector.empty[Cart])

	def findLastRemainingCartPos(carts: Vector[Cart], tracks: Map[Pos, Char])(using Ordering[Pos]): Pos =
		@annotation.tailrec
		def tick(toMove: Vector[Cart], moved: Vector[Cart]): Pos =
			if toMove.isEmpty && moved.nonEmpty then
				tick(moved.sortBy(_.loc), Vector.empty[Cart])
			else if toMove.length == 1 && moved.isEmpty then
				toMove.head.move.loc
			else
				val movedCart      = toMove.head.move
				val remainingCarts = toMove.tail ++ moved

				findCollisionPos(remainingCarts, movedCart) match
					case Some(pos) if remainingCarts.forall(_.loc == pos) => pos
					case Some(pos) => tick(toMove.tail.filterNot(_.loc == pos), moved.filterNot(_.loc == pos))
					case None      => tick(toMove.tail, moved :+ movedCart.turn(tracks(movedCart.loc)))

		tick(carts.sortBy(_.loc), Vector.empty[Cart])

	lazy val pt1 =
		val Pos(x, y) =
			findFirstCollisionPos(carts, tracks)

		s"$x,$y"

	lazy val pt2 =
		val Pos(x, y) =
			findLastRemainingCartPos(carts, tracks)

		s"$x,$y"

	answer(1)(pt1)

	answer(2)(pt2)
