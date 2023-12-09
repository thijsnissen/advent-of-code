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
    items: Vector[Long],
    operation: String,
    test: Long => Boolean,
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
            operation,
            (i: Long) => i % test.toInt == 0,
            ifTrue.toInt,
            ifFalse.toInt,
            0
          )

    def playRound(monkeys: Vector[Monkey], part: Int): Vector[Monkey] =
      monkeys.indices.foldLeft(monkeys): (monkeys: Vector[Monkey], id: Int) =>
        takeTurn(monkeys, id, part)

    def takeTurn(monkeys: Vector[Monkey], id: Int, part: Int): Vector[Monkey] =
      val monkey: Monkey = monkeys(id)

      monkey
        .items
        .foldLeft(monkeys): (monkeys: Vector[Monkey], item: Long) =>
          val inspectedItem: Long = inspectItem(item, monkey.operation, part)

          if monkey.test(inspectedItem) then
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

    def inspectItem(item: Long, operation: String, part: Int): Long =
      // import utilities.Utilities.lcm
      // pprint.log(List(23, 19, 13, 17).reduce(_ lcm _)) // Test
      // pprint.log(List(7, 3, 2, 11, 17, 5, 13, 19).reduce(_ lcm _)) // Prod
      val modulo: Long = if getEnv == Test then 96577 else 9699690
      val divide: Int  = if part == 1 then 3 else 1

      operation match
        case s"new = old * old" => ((item % modulo) * item) / divide
        case s"new = old * $x" => ((item % modulo) * x.toInt) / divide
        case s"new = old + $x" => ((item % modulo) + x.toInt) / divide

    extension (self: Vector[Monkey])
      def monkeyBusiness: Long =
        self
          .sortBy(_.counter)(Ordering[Long].reverse)
          .take(2)
          .productBy(_.counter)

  lazy val pt1: Long =
    Iterator
      .iterate(monkeys)(Monkey.playRound(_, 1))
      .drop(20)
      .next
      .monkeyBusiness

  lazy val pt2: Long =
    Iterator
      .iterate(monkeys)(Monkey.playRound(_, 2))
      .drop(10000)
      .next
      .monkeyBusiness

  answer(1)(pt1)

  answer(2)(pt2)
