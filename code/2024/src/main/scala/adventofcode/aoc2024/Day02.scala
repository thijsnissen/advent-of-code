package adventofcode
package aoc2024

import adventofcode.utilities.AdventOfCode.*

object Day02 extends AdventOfCode(Prod):
  val reports: List[Report] =
    input
      .linesIterator
      .map(Report.fromString)
      .toList

  type Report = List[Int]

  object Report:
    def fromString(s: String): Report =
      s.split(" ").map(_.toInt).toList

    extension (self: Report)
      def problemDampener: List[Report] =
        self
          .indices
          .foldLeft(List(self)): (acc, i) =>
            (self.take(i) ::: self.drop(i + 1)) :: acc

      def checkRules: Boolean =
        val windows = self.sliding(2).toVector

        windows.forall(w => 1 to 3 contains w(0) - w(1)) ||
        windows.forall(w => 1 to 3 contains w(1) - w(0))

      def isSafe(withProblemDampener: Boolean): Boolean =
        @tailrec def loop(todo: List[Report]): Boolean =
          todo.headOption match
            case None => false
            case Some(report) =>
              val result = report.checkRules

              if result then result else loop(todo.tail)

        loop(if withProblemDampener then self.problemDampener else List(self))

  import Report.*

  lazy val pt1: Int =
    reports.count(_.isSafe(withProblemDampener = false))

  lazy val pt2: Int =
    reports.count(_.isSafe(withProblemDampener = true))

  answer(1)(pt1)

  answer(2)(pt2)
