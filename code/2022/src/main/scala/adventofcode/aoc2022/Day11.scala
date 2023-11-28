package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day11 extends AdventOfCode(Prod):
  val monkeys: Vector[Monkey] =
    input
      .linesIterator
      .map(_.trim)
      .grouped(7)
      .map(Monkey.fromInput)
      .toVector

  case class Monkey(
    id: Int,
    items: Vector[Int],
    op: Int => Int,
    test: Int => Int,
    counter: Int = 0
  ):
    def addItem(item: Int): Monkey =
      copy(items = items :+ item)

    def removeItem: Monkey =
      copy(items = if items.nonEmpty then items.tail else items)

    def incrementCounter: Monkey =
      copy(counter = counter + 1)

  case class State(monkeys: Vector[Monkey]):
    def takeTurn(id: Int): State =
      monkeys(id).items.foldLeft(this): (acc: State, i: Int) =>
        // if id == 0 then println(monkeys.map(m => s"${m.id}: ${m.items}").mkString("", "\n", "\n--------"))

        val worryLevel: Int = monkeys(id).op(i)

        acc
          .moveItem(id, monkeys(id).test(worryLevel), worryLevel)
          .incrementCounter(id)

    def moveItem(from: Int, to: Int, item: Int): State =
      val newMonkeys: Vector[Monkey] =
        monkeys
          .updated(to, monkeys(to).addItem(item))
          .updated(from, monkeys(from).removeItem)

      copy(monkeys = newMonkeys)

    def incrementCounter(id: Int): State =
      copy(monkeys = monkeys.updated(id, monkeys(id).incrementCounter))

  object Monkey:
    def fromInput(ss: Seq[String]): Monkey =
      def parseId(s: String): Int =
        s match
          case s"Monkey $id:" => id.toInt

      def parseItems(s: String): Vector[Int] =
        s match
          case s"Starting items: $items" =>
            items.split(", ").map(_.toInt).toVector

      def parseOp(s: String): Int => Int =
        s match
          case s"Operation: new = old $op $i" =>
            getOperation((op, i))

      def parseTest(s: String): Int =
        s match
          case s"Test: divisible by $t" => t.toInt

      def parseIfTrue(s: String): Int =
        s match
          case s"If true: throw to monkey $i" => i.toInt

      def parseIfFalse(s: String): Int =
        s match
          case s"If false: throw to monkey $i" => i.toInt

      Monkey(
        parseId(ss(0)),
        parseItems(ss(1)),
        parseOp(ss(2)),
        getTest.curried(parseTest(ss(3)))(parseIfTrue(ss(4)))(
          parseIfFalse(ss(5))
        )
      )

    def getOperation(op: (String, String)): Int => Int =
      op match
        case ("*", "old") => (x: Int) => (x + x) / 3
        case ("*", i)     => (x: Int) => (x * i.toInt) / 3
        case ("+", i)     => (x: Int) => (x + i.toInt) / 3

    def getTest(test: Int, ifTrue: Int, ifFalse: Int, item: Int): Int =
      if item % test == 0 then ifTrue else ifFalse

  lazy val pt1: Int =
    val round: State => State =
      (s: State) =>
        s.monkeys.indices.foldLeft(s): (acc, id) =>
          acc.takeTurn(id)

    val monkeyBusiness: Int =
      Iterator
        .iterate(State(monkeys))(round)
        .drop(20)
        .next
        .monkeys
        .map(_.counter)
        .sorted
        .takeRight(2)
        .product

    monkeyBusiness

  lazy val pt2: Int = ???

  answer(1)(pt1)

  answer(2)(pt2)
