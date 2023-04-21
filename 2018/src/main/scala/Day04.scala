import scala.io.Source

object Day04 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Vector[Guard] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.toVector
			.sorted
			.foldLeft((0L, Vector.empty[Guard])) {
				case ((_, state), s"[$_] Guard #${guardId} begins shift") =>
					(guardId.toLong, state)
				case ((id, state), s"[${_}:${minutes}] falls asleep") =>
					(id, Guard(guardId = id, from = minutes.toInt) +: state)
				case ((id, state), s"[${_}:${minutes}] wakes up") =>
					(id, state.updated(0, state(0).copy(till = minutes.toInt)))
				case (state, _) => state
			}._2

	private final case class Guard(guardId: Long, from: Int = 0, till: Int = 0):
		val sleepTime: Int = till - from
		val sleepWindow: Vector[Int] = (from until till).toVector

	private val startTimePart1: Long =
		System.currentTimeMillis

	private val guardIdIsMostAsleep: Long =
		input
			.groupMapReduce { case Guard(id, _, _) => id }(g => g.sleepTime)(_ + _)
			.maxBy(_._2)._1

	private val minuteIsMostAsleep: Int =
		input
			.filter(g => g.guardId == guardIdIsMostAsleep)
			.flatMap(g => g.sleepWindow)
			.groupBy(identity)
			.map((x, y) => (x, y.size))
			.maxBy(_._2)
			._1

	val answerPart1 = guardIdIsMostAsleep * minuteIsMostAsleep // test: 240 [1ms] , input: 76357 [1ms]

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	private val mostAsleepOnSameMinute: (Long, Int, Int) =
		input
			.groupMap { case Guard(id, _, _) => id }(_.sleepWindow)
			.map((x, y) => (x, y.flatten.groupBy(identity).maxBy(_._2.size)))
			.map((x, y) => (x, y._1, y._2.size))
			.maxBy(_._3)

	val answerPart2 = mostAsleepOnSameMinute._1 * mostAsleepOnSameMinute._2 // test: 4455 [0ms], input: 41668 [2ms]

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
