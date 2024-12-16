package adventofcode
package aoc2024

import utilities.AdventOfCode.*

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
      def isSafe: Boolean =
        self.sliding(2).forall(ls => 1 to 3 contains ls(0) - ls(1)) ||
          self.sliding(2).forall(ls => 1 to 3 contains ls(1) - ls(0))

      def isSafeWithProblemDampener: Boolean =
        self
          .indices
          .exists: i =>
            (self.take(i) ::: self.drop(i + 1)).isSafe

  import Report.*

  lazy val pt1: Int =
    reports.count(_.isSafe)

  lazy val pt2: Int =
    reports.count: r =>
      r.isSafe || r.isSafeWithProblemDampener

  answer(1)(pt1)

  answer(2)(pt2)
