import scala.io.Source
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object Day07 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: List[Step[Char]] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.collect:
				case s"Step $dependsOn must be finished before step $value can begin." =>
					Step(value.head, dependsOn.head)
			.toList

	private case class Step[A](value: A, dependsOn: A)

	private def orderSteps[A](steps: List[Step[A]], timer: A => Int, workers: Int)(using Ordering[A]): (List[A], Int) =
		case class Workload(
			queue: SortedSet[A] = SortedSet.empty[A],
			working: SortedMap[A, Int] = SortedMap.empty[A, Int],
			completed: List[A] = List.empty[A]
			):
			def isDone: Boolean =
				queue.isEmpty && working.isEmpty

			def process(edgesWithNoDeps: SortedMap[A, Set[A]], currentTime: Int): Workload =
				val (newWorking, newQueue) =
					(queue ++ edgesWithNoDeps.keys)
						.splitAt(workers - working.size)

				val (done, notDone) =
					(working ++ newWorking.map(s => s -> (currentTime + timer(s) - 1)))
						.partition((_, timeDone) => timeDone == currentTime)

				Workload(newQueue, notDone, completed ++ done.keys)

		val edges: SortedMap[A, Set[A]] = steps.foldLeft(SortedMap.empty[A, Set[A]]):
			(acc, edge) =>
				acc + (edge.dependsOn -> acc.getOrElse(edge.dependsOn, Set())) +
					(edge.value -> (acc.getOrElse(edge.value, Set()) + edge.dependsOn))

		@annotation.tailrec
		def go(
						edges: SortedMap[A, Set[A]],
						workload: Workload,
						currentTime: Int
					): (List[A], Int) =
			val (noDeps, hasDeps) = edges.partition((_, deps) => deps.isEmpty)

			if noDeps.isEmpty && workload.isDone then
				(workload.completed, currentTime)
			else
				val newWorkload =
					workload.process(noDeps, currentTime)

				val newEdges =
					hasDeps.map((key, deps) => key -> (deps -- newWorkload.completed))

				go(newEdges, newWorkload, currentTime + 1)

		go(edges, Workload(), currentTime = 0)

	private val startTimePart1: Long =
		System.currentTimeMillis

	private val (order, _) = orderSteps(input, timer = _ => 1, workers = 1)

	val answerPart1 = order.mkString("")

	// test: CABDFE [1ms], input: IBJTUWGFKDNVEYAHOMPCQRLSZX [2ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	private val (_, timeToComplete) = orderSteps(input, timer = _.toInt - 4, workers = 5)

	val answerPart2 = timeToComplete

	// test: 15 [0ms], input: 1118 [4ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
