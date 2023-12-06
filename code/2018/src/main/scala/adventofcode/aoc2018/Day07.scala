package adventofcode
package aoc2018

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import utilities.AdventOfCode.*

object Day07 extends AdventOfCode(Prod):
  val myInput: List[Step[Char]] =
    input
      .linesIterator
      .collect:
        case s"Step $dependsOn must be finished before step $value can begin." =>
          Step(value.head, dependsOn.head)
      .toList

  case class Step[A](value: A, dependsOn: A)

  def orderSteps[A](steps: List[Step[A]], timer: A => Int, workers: Int)(using
    Ordering[A]
  ): (List[A], Int) =
    case class Workload(
      queue: SortedSet[A] = SortedSet.empty[A],
      working: SortedMap[A, Int] = SortedMap.empty[A, Int],
      completed: List[A] = List.empty[A]
    ):
      def isDone: Boolean =
        queue.isEmpty && working.isEmpty

      def process(
        edgesWithNoDeps: SortedMap[A, Set[A]],
        currentTime: Int
      ): Workload =
        val (newWorking, newQueue) =
          (queue ++ edgesWithNoDeps.keys)
            .splitAt(workers - working.size)

        val (done, notDone) =
          (working ++ newWorking.map(s => s -> (currentTime + timer(s) - 1)))
            .partition((_, timeDone) => timeDone == currentTime)

        Workload(newQueue, notDone, completed ++ done.keys)

    val edges: SortedMap[A, Set[A]] =
      steps.foldLeft(SortedMap.empty[A, Set[A]]): (acc, edge) =>
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

  lazy val pt1: String =
    val (order, _) =
      orderSteps(myInput, timer = _ => 1, workers = 1)

    order.mkString("")

  lazy val pt2: Int =
    val (_, timeToComplete) =
      orderSteps(myInput, timer = _.toInt - 4, workers = 5)

    timeToComplete

  answer(1)(pt1)

  answer(2)(pt2)
