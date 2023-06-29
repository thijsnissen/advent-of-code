// import scala.io.Source
//
// object Day13 extends App:
// 	val day: String =
// 		this.getClass.getName.dropRight(1).toLowerCase
//
// 	val input: Vector[Track | Cart] =
// 		Source
// 			.fromResource(s"$day-test.txt")
// 			.getLines
// 			.toVector
// 			.zipWithIndex
// 			.flatMap:
// 				case (l, y) => l.zipWithIndex.foldLeft(Vector.empty[Track | Cart]):
// 					case (acc, ('-', x))  => acc :+ Track(Pos(x, y), Path.Straight)
// 					case (acc, ('|', x))  => acc :+ Track(Pos(x, y), Path.Straight)
// 					case (acc, ('/', x))  => acc :+ Track(Pos(x, y), Path.Curve)
// 					case (acc, ('\\', x)) => acc :+ Track(Pos(x, y), Path.Curve)
// 					case (acc, ('+', x))  => acc :+ Track(Pos(x, y), Path.Intersection)
// 					case (acc, ('<', x))  => acc :+ Cart(Pos(x, y), Direction.Left, 0) :+ Track(Pos(x, y), Path.Straight)
// 					case (acc, ('>', x))  => acc :+ Cart(Pos(x, y), Direction.Right, 0) :+ Track(Pos(x, y), Path.Straight)
// 					case (acc, ('^', x))  => acc :+ Cart(Pos(x, y), Direction.Up, 0) :+ Track(Pos(x, y), Path.Straight)
// 					case (acc, ('v', x))  => acc :+ Cart(Pos(x, y), Direction.Down, 0) :+ Track(Pos(x, y), Path.Straight)
// 					case (acc, _) => acc
//
// 	enum Path:
// 		case Curve, Straight, Intersection
//
// 	enum Direction(pos: Pos):
// 		case Left  extends Direction(Pos(-1, 0))
// 		case Right extends Direction(Pos(1, 0))
// 		case Up    extends Direction(Pos(0, -1))
// 		case Down  extends Direction(Pos(0, 1))
//
// 	case class Pos(x: Int, y: Int):
// 		self =>
// 			@annotation.targetName("plus")
// 			def +(that: Pos): Pos =
// 				Pos(self.x + that.x, self.y + that.y)
//
// 			def flip: Pos
// 				Pos(self.y * -1, self.x * -1)
//
// 	case class Track(pos: Pos, path: Path)
//
// 	case class Cart(pos: Pos, direction: Direction, turns: Int):
// 		def turn: Cart =
// 			turns % 3 match
// 				case 0 => copy(direction = ???, turns = turns + 1)
// 				case 1 => copy(direction = ???, turns = turns + 1)
// 				case 2 => copy(direction = ???, turns = turns + 1)
//
// 	object Cart:
// 		@annotation.tailrec
// 		def tick(carts: Vector[Cart], tracks: Vector[Track]): Pos =
// 			if hasCrash(carts) then
// 				getCrash(carts)
// 			else
// 				val newCarts =
// 					carts.map:
// 						c =>
// 							val t = tracks.find(_.pos == c.pos)
// 							c.copy(pos = c.pos + c.direction.pos, t.path.direction, c.turns)
//
//
// 				tick(newCarts, tracks)
//
// 		def hasCrash(carts: Vector[Cart]): Boolean =
// 				carts
// 					.groupBy(_.pos)
// 					.map:
// 						case (_, carts) if carts.size > 1 => true
// 						case _ => false
// 					.toVector
// 					.contains(true)
//
// 		def getCrash(carts: Vector[Cart]): Pos =
// 			val (pos: Pos, _) =
// 				carts
// 					.groupBy(_.pos)
// 					.maxBy:
// 						case (_, carts) => carts.size
//
// 			pos
//
// 	val startTimePart1: Long =
// 		System.currentTimeMillis
//
// 	val carts: Vector[Cart] =
// 		input.collect:
// 			case i: Cart => i
//
// 	val tracks: Vector[Track] =
// 		input.collect:
// 			case i: Track => i
//
// 	val posOfFirstCrash: Option[Pos] = Cart.tick(carts, tracks).getOrElse(sys.error("No crash found"))
//
// 	val answerPart1 = s"${posOfFirstCrash.x},${posOfFirstCrash.y}"
//
// 	// test: , input:
// 	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")
//
// 	val startTimePart2: Long =
// 		System.currentTimeMillis
//
// 	val answerPart2 = ???
//
// 	// test: , input:
// 	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
