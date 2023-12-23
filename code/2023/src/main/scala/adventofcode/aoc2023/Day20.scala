package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day20 extends AdventOfCode(Test):
  val moduleConfiguration: Map[String, Module] =
    input
      .linesIterator
      .map(Module.fromString)
      .toMap

  trait Module:
    val destinationModules: List[String]

  object Module:
    def fromString(s: String): (String, Module) =
      s match
        case s"broadcaster -> $d" => "broadcaster" -> Broadcast(d.split(",").toList)
        case s"%$n -> $d" => n -> FlipFlop(d.split(",").toList, 0)
        case s"&$n -> $d" => n -> Conjunction(d.split(",").toList, d.split(",").map(_ -> 0).toMap)

  case class FlipFlop(destinationModules: List[String], state: Int) extends Module
    // fips state when recieved 0 and then sends new state.

  case class Conjunction(destinationModules: List[String], state: Map[String, Int]) extends Module
      // when recieving update state for sender, if all states are high send low else send hi

  case class Broadcast(destinationModules: List[String]) extends Module
    // sends the same as it recieves

  case class ButtonModule(state: Map[String, Module], low: Int = 0, high: Int = 0):
    def apty: ButtonModule =
      ???

  lazy val pt1: Int =
    val buttonModule: Iterator[ButtonModule] =
      Iterator.iterate(ButtonModule(moduleConfiguration))(_.apty)

    val ButtonModule(_, low, high) =
      buttonModule
        .drop(1000)
        .next

    low * high

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
