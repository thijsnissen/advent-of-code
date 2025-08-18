package adventofcode
package aoc2024

import utilities.AdventOfCode.*

object Day03 extends AdventOfCode(Prod):
  val program: String = input

  def multiply(s: String)(withConditionals: Boolean): Int =
    @tailrec def loop(todo: String, acc: Int = 0): Int =
      todo match
        case ""                                  => acc
        case s"don't()$tail" if withConditionals =>
          loop(tail.drop(tail.indexOf("do()")), acc)
        case s"mul($x,$y)$tail" if x.isInt && y.isInt =>
          loop(tail, acc + x.toInt * y.toInt)
        case _ => loop(todo.tail, acc)

    loop(s)

  extension (self: String)
    def isInt: Boolean =
      self
        .toIntOption
        .exists((0 to 999).contains)

  lazy val pt1: Int =
    multiply(program)(withConditionals = false)

  lazy val pt2: Int =
    multiply(program)(withConditionals = true)

  answer(1)(pt1)

  answer(2)(pt2)
