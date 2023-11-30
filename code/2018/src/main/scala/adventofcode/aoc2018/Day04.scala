package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day04 extends AdventOfCode(Prod):
  val myInput: Vector[Guard] =
    input
      .linesIterator
      .toVector
      .sorted
      .foldLeft((0L, Vector.empty[Guard])):
        case ((_, state), s"[$_] Guard #${guardId} begins shift") =>
          (guardId.toLong, state)
        case ((id, state), s"[${_}:${minutes}] falls asleep") =>
          (id, Guard(guardId = id, from = minutes.toInt) +: state)
        case ((id, state), s"[${_}:${minutes}] wakes up") =>
          (id, state.updated(0, state(0).copy(till = minutes.toInt)))
        case (state, _) => state
      ._2

  case class Guard(guardId: Long, from: Int = 0, till: Int = 0):
    val sleepTime: Int           = till - from
    val sleepWindow: Vector[Int] = (from until till).toVector

  def guardIdIsMostAsleep: Long =
    myInput
      .groupMapReduce {
        case Guard(id, _, _) => id
      }(g => g.sleepTime)(_ + _)
      .maxBy(_._2)._1

  def minuteIsMostAsleep: Int =
    myInput
      .filter(g => g.guardId == guardIdIsMostAsleep)
      .flatMap(g => g.sleepWindow)
      .groupBy(identity)
      .map((x, y) => (x, y.size))
      .maxBy(_._2)
      ._1

  def mostAsleepOnSameMinute: (Long, Int, Int) =
    myInput
      .groupMap:
        case Guard(id, _, _) => id
      .apply(_.sleepWindow)
      .map((x, y) => (x, y.flatten.groupBy(identity).maxBy(_._2.size)))
      .map((x, y) => (x, y._1, y._2.size))
      .maxBy(_._3)

  lazy val pt1: Long =
    guardIdIsMostAsleep * minuteIsMostAsleep

  lazy val pt2: Long =
    mostAsleepOnSameMinute._1 * mostAsleepOnSameMinute._2

  answer(1)(pt1)

  answer(2)(pt2)
