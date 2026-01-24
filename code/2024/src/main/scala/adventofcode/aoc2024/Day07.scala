package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Utilities.sumBy

object Day07 extends AdventOfCode(Prod):
  val calibrationEquations: Vector[Equation] =
    input
      .linesIterator
      .map(Equation.fromString)
      .toVector

  enum Operator:
    case Add, Mul, Con

    def run(a: Long, b: Long): Long =
      this match
        case Operator.Add => a + b
        case Operator.Mul => a * b
        case Operator.Con => s"$a$b".toLong

  object Operator:
    def combinations(i: Int, ops: Int): Iterator[List[Int]] =
      Iterator.tabulate(math.pow(ops, i).toInt): c =>
        BigInt(c)
          .toString(ops)
          .reverse
          .padTo(i, '0')
          .map(_.asDigit)
          .toList

  case class Equation(test: Long, equations: List[Long]):
    def isTrue(ops: Vector[Operator]): Boolean =
      @tailrec def loop(todo: List[Long], comb: List[Int], acc: Long): Long =
        (todo, comb) match
          case (h :: t, ch :: ct) => loop(t, ct, ops(ch).run(acc, h))
          case _                  => acc

      Operator
        .combinations(equations.length - 1, ops.length)
        .exists(loop(equations.tail, _, equations.head) == test)

  object Equation:
    def fromString(s: String): Equation =
      s match
        case s"$test: $equations" =>
          Equation(test.toLong, equations.split(" ").map(_.toLong).toList)

  override lazy val pt1: Long =
    calibrationEquations
      .filter(_.isTrue(ops = Vector(Operator.Add, Operator.Mul)))
      .sumBy(_.test)

  override lazy val pt2: Long =
    calibrationEquations
      .filter(_.isTrue(ops = Vector(Operator.Add, Operator.Mul, Operator.Con)))
      .sumBy(_.test)
