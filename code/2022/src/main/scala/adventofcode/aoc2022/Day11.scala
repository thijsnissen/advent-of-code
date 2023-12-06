package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day11 extends AdventOfCode(Test):
  val monkeys: Vector[Monkey] =
    input
      .linesIterator
      .filter(_.nonEmpty)
      .grouped(6)
      .map(Monkey.fromInput)
      .toVector

  case class Monkey(
    id: Int,
    items: Vector[Int],
    operation: Int => Int,
    test: Int => Boolean,
    ifTrue: Int,
    ifFalse: Int,
    counter: Int,
  ):
    def takeTurn(monkeys: Vector[Monkey]): Vector[Monkey] =
      items
        .foldLeft(monkeys): (monkeys: Vector[Monkey], item: Int) =>
          val inspectedItem: Int = operation(item) / 3

          if test(inspectedItem) then
            monkeys.updated(ifTrue, monkeys(ifTrue).copy(items = monkeys(ifTrue).items :+ inspectedItem))
          else
            monkeys.updated(ifFalse, monkeys(ifFalse).copy(items = monkeys(ifFalse).items :+ inspectedItem))
        .updated(id, monkeys(id).copy(items = Vector.empty[Int], counter = counter + items.length))

  object Monkey:
    def fromInput(monkey: Seq[String]): Monkey =
      def parseOperation(s: String): Int => Int =
        s match
          case s"new = old * old" => (i: Int) => i * i
          case s"new = old * $x"  => (i: Int) => i * x.toInt
          case s"new = old + $x"  => (i: Int) => i + x.toInt

      monkey.map(_.trim) match
        case Seq(
              s"Monkey $id:",
              s"Starting items: $items",
              s"Operation: $operation",
              s"Test: divisible by $test",
              s"If true: throw to monkey $ifTrue",
              s"If false: throw to monkey $ifFalse",
            ) =>
          Monkey(
            id.toInt,
            items.split(", ").map(_.toInt).toVector,
            parseOperation(operation),
            (i: Int) => i % test.toInt == 0,
            ifTrue.toInt,
            ifFalse.toInt,
            0,
          )

    def playRound(monkeys: Vector[Monkey]): Vector[Monkey] =
      // Next monkey should be picked from state.
      monkeys.foldLeft(monkeys): (monkeys: Vector[Monkey], monkey: Monkey) =>
        monkey.takeTurn(monkeys)

  lazy val pt1: Int =
    Monkey
      .playRound(monkeys)
      .map(_.counter)
      .sorted
      .take(2)
      .product

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
