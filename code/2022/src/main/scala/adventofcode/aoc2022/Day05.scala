package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day05 extends AdventOfCode(Prod):
  val startingInput =
    input.linesIterator.toVector

  import scala.collection.immutable.SortedMap

  opaque type Stacks =
    SortedMap[Int, List[Char]]

  object Stacks:
    def unit: Stacks =
      SortedMap.empty[Int, List[Char]]
        .withDefaultValue(List.empty[Char])

  case class Instruction(count: Int, from: Int, to: Int):
    def executeByOne(stack: Stacks): Stacks =
      stack
        .updated(to, stack(from).take(count).reverse ++ stack(to))
        .updated(from, stack(from).drop(count))

    def executeAtOnce(stack: Stacks): Stacks =
      stack
        .updated(to, stack(from).take(count) ++ stack(to))
        .updated(from, stack(from).drop(count))

  val startingStack: Stacks =
    startingInput
      .filter(_.contains('['))
      .flatMap(_.grouped(4).map(_.charAt(1)).zipWithIndex)
      .filterNot((c, _) => c == ' ')
      .reverse
      .foldLeft(Stacks.unit):
        case (acc, (crate, index)) =>
          acc + (index + 1 -> (crate :: acc(index + 1)))

  val instructions: Vector[Instruction] =
    startingInput
      .filter(_.startsWith("move"))
      .collect:
        case s"move $count from $from to $to" =>
          Instruction(count.toInt, from.toInt, to.toInt)

  lazy val pt1: String =
    instructions
      .foldLeft(startingStack): (s, i) =>
        i.executeByOne(s)
      .map((_, c) => c.head)
      .mkString

  lazy val pt2: String =
    instructions
      .foldLeft(startingStack): (s, i) =>
        i.executeAtOnce(s)
      .map((_, c) => c.head)
      .mkString

  answer(1)(pt1)

  answer(2)(pt2)
