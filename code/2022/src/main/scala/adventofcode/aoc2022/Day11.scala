package adventofcode
package aoc2022

import utilities.AdventOfCode.*
import utilities.Utilities.lcm
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
    items: Vector[Long],
    operation: Long => Long,
    test: Int,
    ifTrue: Int,
    ifFalse: Int,
    counter: Long
  )

  object Monkey:
    def fromInput(monkey: Seq[String]): Monkey =
      monkey.map(_.trim) match
        case Seq(
              s"Monkey $_:",
              s"Starting items: $items",
              s"Operation: $operation",
              s"Test: divisible by $test",
              s"If true: throw to monkey $ifTrue",
              s"If false: throw to monkey $ifFalse"
            ) =>
          Monkey(
            items.split(", ").map(_.toLong).toVector,
            parseOperation(operation),
            test.toInt,
            ifTrue.toInt,
            ifFalse.toInt,
            0
          )

    def parseOperation(s: String): Long => Long =
      s match
        case s"new = old * old" => (i: Long) => i * i
        case s"new = old * $x"  => (i: Long) => i * x.toInt
        case s"new = old + $x"  => (i: Long) => i + x.toInt

    def playRound(
      monkeys: Vector[Monkey],
      modulo: Long,
      isPartTwo: Boolean = false
    ): Vector[Monkey] =
      monkeys.indices.foldLeft(monkeys): (monkeys: Vector[Monkey], id: Int) =>
        takeTurn(monkeys, id, modulo, isPartTwo)

    def takeTurn(
      monkeys: Vector[Monkey],
      id: Int,
      modulo: Long,
      isPartTwo: Boolean
    ): Vector[Monkey] =
      val monkey: Monkey = monkeys(id)

      monkey
        .items
        .foldLeft(monkeys): (monkeys: Vector[Monkey], item: Long) =>
          val inspectedItem: Long =
            if isPartTwo then monkey.operation(item % modulo)
            else monkey.operation(item % modulo) / 3

          if inspectedItem % monkey.test == 0 then
            monkeys.updated(
              monkey.ifTrue,
              monkeys(monkey.ifTrue).copy(items =
                monkeys(monkey.ifTrue).items :+ inspectedItem
              )
            )
          else
            monkeys.updated(
              monkey.ifFalse,
              monkeys(monkey.ifFalse).copy(items =
                monkeys(monkey.ifFalse).items :+ inspectedItem
              )
            )
        .updated(
          id,
          monkey.copy(
            items = Vector.empty[Long],
            counter = monkey.counter + monkey.items.length
          )
        )

    extension (self: Vector[Monkey])
      def monkeyBusiness: Long =
        self
          .sortBy(_.counter)(Ordering[Long].reverse)
          .take(2)
          .productBy(_.counter)

  lazy val pt1: Long =
    Iterator
      .iterate(monkeys):
        Monkey.playRound(_, monkeys.map(_.test).reduce(_ lcm _))
      .drop(20)
      .next
      .monkeyBusiness

  lazy val pt2: Long =
    Iterator
      .iterate(monkeys):
        Monkey.playRound(_, monkeys.map(_.test).reduce(_ lcm _), true)
      .drop(10000)
      .next
      .monkeyBusiness

  answer(1)(pt1)

  answer(2)(pt2)
