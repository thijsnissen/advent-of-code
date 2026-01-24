package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Utilities.sumBy

object Day05 extends AdventOfCode(Prod):
  val (pageOrderingRules: Rules, pagesToProduce: Vector[Update]) =
    val Array(rules, pages) =
      input.split("\n\n").map(_.linesIterator)

    (
      rules
        .map(Rules.fromString)
        .toSet
        .groupMap((b, _) => b)((_, a) => a),
      pages
        .map(Update.fromString)
        .toVector
    )

  type Rules = Map[Int, Set[Int]]

  object Rules:
    def fromString(s: String): (Int, Int) =
      s match
        case s"$b|$a" => (b.toInt, a.toInt)

  type Update = Vector[Int]

  object Update:
    def fromString(s: String): Vector[Int] =
      s.split(",").map(_.toInt).toVector

    extension (self: Vector[Update])
      def partitioned(rs: Rules): (Vector[Update], Vector[Update]) =
        self.partition(_.isCorrect(rs))

      def sumMiddlePages: Int =
        self.sumBy(u => u(u.length / 2))

    extension (self: Update)
      def order(rs: Rules): Update =
        self.sorted(using
          (x: Int, y: Int) =>
            if rs.get(x).exists(_.contains(y)) then -1
            else if rs.get(y).exists(_.contains(x)) then 1
            else 0
        )

      def isCorrect(rs: Rules): Boolean =
        self == self.order(rs)

  import Update.*

  override lazy val pt1: Int =
    val (correctlyOrdered, _) =
      pagesToProduce.partitioned(pageOrderingRules)

    correctlyOrdered.sumMiddlePages

  override lazy val pt2: Int =
    val (_, incorrectlyOrdered) =
      pagesToProduce.partitioned(pageOrderingRules)

    incorrectlyOrdered
      .map(_.order(pageOrderingRules))
      .sumMiddlePages
