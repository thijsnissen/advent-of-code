package adventofcode
package aoc2022

import utilities.AdventOfCode.*
import utilities.Utilities.productBy

object Day11 extends AdventOfCode(Prod):
  val monkeys: Vector[Monkey] =
    input
      .linesIterator
      .filter(_.nonEmpty)
      .grouped(6)
      .map(Monkey.fromInput)
      .toVector

  case class Monkey(
    id: Int,
    items: Vector[Long],
    operation: Long => Long,
    test: Long => Boolean,
    ifTrue: Int,
    ifFalse: Int,
    counter: Long
  ):
    def takeTurn(monkeys: Vector[Monkey]): Vector[Monkey] =
      items
        .foldLeft(monkeys): (monkeys: Vector[Monkey], item: Long) =>
          val inspectedItem: Long = operation(item) / 3

          if test(inspectedItem) then
            monkeys.updated(
              ifTrue,
              monkeys(ifTrue).copy(items =
                monkeys(ifTrue).items :+ inspectedItem
              )
            )
          else
            monkeys.updated(
              ifFalse,
              monkeys(ifFalse).copy(items =
                monkeys(ifFalse).items :+ inspectedItem
              )
            )
        .updated(
          id,
          monkeys(id).copy(
            items = Vector.empty[Long],
            counter = counter + items.length
          )
        )

  object Monkey:
    def fromInput(monkey: Seq[String]): Monkey =
      def parseOperation(s: String): Long => Long =
        s match
          case s"new = old * old" => (i: Long) => i * i
          case s"new = old * $x"  => (i: Long) => i * x.toInt
          case s"new = old + $x"  => (i: Long) => i + x.toInt

      monkey.map(_.trim) match
        case Seq(
              s"Monkey $id:",
              s"Starting items: $items",
              s"Operation: $operation",
              s"Test: divisible by $test",
              s"If true: throw to monkey $ifTrue",
              s"If false: throw to monkey $ifFalse"
            ) =>
          Monkey(
            id.toInt,
            items.split(", ").map(_.toLong).toVector,
            parseOperation(operation),
            (i: Long) => i % test.toInt == 0,
            ifTrue.toInt,
            ifFalse.toInt,
            0
          )

    def playRound(monkeys: Vector[Monkey]): Vector[Monkey] =
      monkeys.indices.foldLeft(monkeys): (monkeys: Vector[Monkey], i: Int) =>
        monkeys(i).takeTurn(monkeys)

  lazy val pt1: Long =
    Iterator
      .iterate(monkeys)(Monkey.playRound)
      .drop(20)
      .next
      .sortBy(_.counter)(Ordering[Long].reverse)
      .take(2)
      .productBy(_.counter)

  lazy val pt2: Long =
    Iterator
      .iterate(monkeys)(Monkey.playRound)
      .drop(10000)
      .next
      .sortBy(_.counter)(Ordering[Long].reverse)
      .take(2)
      .productBy(_.counter)

  answer(1)(pt1)

  answer(2)(pt2)
