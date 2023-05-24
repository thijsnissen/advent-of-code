import scala.io.Source

object Day09 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: List[Game] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.map(Game.fromString)
			.toList

	case class Game(players: Map[Int, Long], player: Int, game: Vector[Long], lastMarble: Long):
		def play(multiplier: Int = 1): Game =
			(1L to lastMarble * multiplier).foldLeft(this):
				case (g, m) if m % 23 == 0 =>
					val (score, board) = g.removeMarble(-7)

					g.copy(
						players = g.players.updatedWith(g.player)(_.map(_ + m + score)),
						player = g.nextPlayer,
						game = board
					)
				case (g, m) =>
					g.copy(player = g.nextPlayer, game = g.addMarble(m))

		private def addMarble(value: Long): Vector[Long] =
			val (l, r) = game.splitAt(2)

			(value +: r) ++ l

		private def removeMarble(index: Long): (Long, Vector[Long]) =
			val (l, r) = game.splitAt((((index % game.length) + game.length) % game.length).toInt)

			(r.head, r.tail ++ l)

		private def nextPlayer: Int =
			player % players.size + 1

	private object Game:
		def fromString(s: String): Game =
			s match
				case s"$players players; last marble is worth $lastMarble points" =>
					Game((1 to players.toInt).map((_, 0L)).toMap, player = 1, Vector[Long](0), lastMarble.toLong)

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 =
		input
			.head
			.play()
			.players
			.maxBy((_, score) => score)
			._2

	// test: 32 [1ms], input: 388024 [17ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
	System.currentTimeMillis

	val answerPart2 =
		input
			.head
			.play(100)
			.players
			.maxBy((_, score) => score)
			._2

	// test: 22563 [2ms] , input: 3180929875 [1486ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
