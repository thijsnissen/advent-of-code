import scala.io.Source

object Day07 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: Vector[Step] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.foldLeft(Vector.empty[Step]):
				case (state, s"Step $a must be finished before step $b can begin.") =>
					if ! state.exists(_.value == a.head) && ! state.exists(_.value == b.head) then
						state :+ Step(a.head) :+ Step(b.head, Vector(a.head))
					else if ! state.exists(_.value == b.head) then
						state :+ Step(b.head, Vector(a.head))
					else
						val i = state.indexWhere(_.value == b.head)
						state.updated(i, state(i).copy(needsAvailable = state(i).needsAvailable :+ a.head))
				case (state, _) => state

	private case class Step(
		value: Char,
		needsAvailable: Vector[Char] = Vector.empty[Char],
	):
		def timeToComplete: Int =
			value.toInt - 64

	private object Step:
		private def getStepsForChars(chars: Vector[Char]): Vector[Step] =
			input.filter(s => chars.contains(s.value))

	private case class Worker(task: Option[Step], timeFinished: Option[Int]):
		def isIdle: Boolean =
			task.isEmpty && timeFinished.isEmpty

	private def completeInstructions(workerCount: Int = 1, baseTime: Int = 0): (String, Int) =
		@annotation.tailrec
		def go(
			steps: Vector[Step],
			workers: Vector[Worker],
			currentTime: Int,
			order: Vector[Char]
		): (String, Int) =
			if steps.isEmpty && workers.forall(_.isIdle) then
				(order.mkString, currentTime - 1)
			else
				var newSteps = steps
				var newOrder = order
				var activeSteps = workers.flatMap(_.task)
				val newWorkers = workers.map: w =>
					pprint.log(w.task.getOrElse(Step('.')).value)
					if w.isIdle && newSteps.nonEmpty then
						val newStep = newSteps.minBy(_.value)
						newSteps = newSteps.filterNot(_ == newStep)
						activeSteps = activeSteps :+ newStep

						Worker(Some(newStep), Some(currentTime + newStep.timeToComplete + baseTime))
					else if w.timeFinished.nonEmpty && w.timeFinished.get == currentTime then
						newOrder = newOrder :+ w.task.get.value
						activeSteps = activeSteps.filter(_ == w)
						newSteps = input
							.filterNot(s => newOrder.contains(s.value))
							.filter(_.needsAvailable.forall(newOrder.contains))
							.filterNot(activeSteps.contains)

						if newSteps.nonEmpty then
							val newStep = newSteps.minBy(_.value)
							newSteps = newSteps.filterNot(_ == newStep)
							activeSteps = activeSteps :+ newStep

							Worker(Some(newStep), Some(currentTime + newStep.timeToComplete + baseTime))
						else
							Worker(None, None)
					else
						w

				go(newSteps, newWorkers, currentTime + 1, newOrder)

		go(
			input.filter(_.needsAvailable.isEmpty),
			Vector.fill(workerCount)(Worker(None, None)),
			0,
			Vector.empty[Char]
		)

	private val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 = completeInstructions(5, 60)._1 // test: CABDFE [1ms], input: IBJTUWGFKDNVEYAHOMPCQRLSZX [1ms]

	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = ??? // test: , input:

	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
