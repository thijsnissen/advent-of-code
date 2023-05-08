import scala.io.Source
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object Day07 extends App:
	private val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	private val input: List[(Char, Char)] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.collect:
				case s"Step $x must be finished before step $y can begin." =>
					(x.head, y.head)
			.toList

	private def orderSteps[A](steps: List[(A, A)], timer: A => Int, workers: Int)(using Ordering[A]): (List[A], Int) =
		case class Workload(
			queue: SortedSet[A] = SortedSet.empty[A],
			working: SortedMap[A, Int] = SortedMap.empty[A, Int],
			completed: List[A] = List.empty[A]
		):
			def isDone: Boolean =
				queue.isEmpty && working.isEmpty

			def process(edgesNoDeps: SortedMap[A, Set[A]], currentTime: Int): Workload =
				val (newWorking, newQueue) =
					(queue ++ edgesNoDeps.keys)
						.splitAt(workers - working.size)

				val (done, notDone) =
					(working ++ newWorking.map(s => s -> (currentTime + timer(s) - 1)))
						.partition(_._2 == currentTime)

				Workload(newQueue, notDone, completed ++ done.keys)

		val edges: SortedMap[A, Set[A]] = steps.foldLeft(SortedMap.empty[A, Set[A]]):
			(acc, edge) =>
				acc + (edge._1 -> acc.getOrElse(edge._1, Set())) +
					(edge._2 -> (acc.getOrElse(edge._2, Set()) + edge._1))

		@annotation.tailrec
		def go(
			edges: SortedMap[A, Set[A]],
			workload: Workload,
			currentTime: Int
		): (List[A], Int) =
			val (noDeps, hasDeps) = edges.partition(_._2.isEmpty)

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

	val answerPart1 = orderSteps(input, timer = _ => 1, workers = 1)._1.mkString("")

	// test: CABDFE [1ms], input: IBJTUWGFKDNVEYAHOMPCQRLSZX [2ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	private val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 = orderSteps(input, timer = _.toInt - 4, workers = 5)._2

	// test: 15 [0ms], input: 1118 [4ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
