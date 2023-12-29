package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day25 extends AdventOfCode(Test):
  val components: Set[Component] =
    input
      .linesIterator
      .flatMap(Component.fromString)
      .toSet

  case class Component(from: Set[String], to: Set[String]):
    def contract: Set[String] = from ++ to

  object Component:
    def fromString(s: String): Set[Component] =
      s match
        case s"$from: $to" =>
          to.split(" ").map((t: String) => Component(Set(from), Set(t))).toSet

  lazy val pt1: Int =
    pprint.log(components)

    123

  answer(1)(pt1)
