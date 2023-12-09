package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day09 extends AdventOfCode(Prod):
  val OASISReport: List[History] =
    input
      .linesIterator
      .map(History.fromString)
      .toList

  type History = List[Int]

  object History:
    def fromString(s: String): History =
      s.split(" ").map(_.toInt).toList

    def extrapolate(history: List[History])(fn: (History, Int) => Int): Int =
      history.tail.foldLeft(List(0)): (acc: History, history: History) =>
        fn(history, acc.head) :: acc
      .head

    def future(history: List[History]): Int =
      extrapolate(history)((h: History, f: Int) => h.reverse.head + f)

    def past(history: List[History]): Int =
      extrapolate(history)((h: History, p: Int) => h.head - p)

    extension (self: History)
      def make(fn: List[History] => Int): Int =
        @tailrec def loop(acc: List[History]): Int =
          if acc.head.forall(_ == 0) then fn(acc)
          else
            loop(
              acc.head.sliding(2).map((h: History) => h(1) - h(0)).toList :: acc
            )

        loop(List(self))

  import History.*

  lazy val pt1: Int =
    OASISReport.map(_.make(future)).sum

  lazy val pt2: Int =
    OASISReport.map(_.make(past)).sum

  answer(1)(pt1)

  answer(2)(pt2)
