package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Utilities.lcm

object Day08 extends AdventOfCode(Prod):
  val instructions: Iterator[Char] =
    Iterator
      .continually(input.takeWhile(_ != '\n'))
      .flatten

  val nodes: Map[String, (String, String)] =
    input
      .dropWhile(_ != '\n')
      .linesIterator
      .collect:
        case s"$node = ($left, $right)" => node -> (left, right)
      .toMap

  @tailrec
  def loop(
    node: String,
    instructions: Iterator[Char],
    nodes: Map[String, (String, String)],
    acc: Int = 0
  ): Long =
    val next: String =
      instructions.next match
        case 'L' => nodes(node)(0)
        case 'R' => nodes(node)(1)

    if next.endsWith("Z") then acc + 1
    else loop(next, instructions, nodes, acc + 1)

  lazy val pt1: Long =
    loop(if getEnv == Test then "22A" else "AAA", instructions, nodes)

  lazy val pt2: Long =
    nodes
      .keys
      .filter(_.endsWith("A"))
      .map(loop(_, instructions, nodes))
      .reduce(_ lcm _)

  answer(1)(pt1)

  answer(2)(pt2)
