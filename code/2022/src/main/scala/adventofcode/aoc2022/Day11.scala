package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day11 extends AdventOfCode(Test):
  val myInput =
    input
      .linesIterator

  case class Monkey(
    startingItems: Vector[Int],
    operation: Int => Int,
    test: Int => Boolean,
    ifTrue: Int,
    ifFalse: Int,
    counter: Int,
  )
  // def takeTurn(monkeys: Vector[Monkey]): Vector[Monkey] =
  //  startingItems.map: (i: Int) =>
  //    if test(operation(i) / 3) then // new items at end
  //      ifTrue
  //    else
  //      ifFalse
  //
  //  this.copy(startingItems = Vector.empty[Int])

  lazy val pt1: Int =
    ???

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
