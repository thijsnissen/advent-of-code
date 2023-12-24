package adventofcode
package aoc2023

import utilities.AdventOfCode.*

// format: off
object Day20 extends AdventOfCode(Test):
  val moduleConfiguration: Map[String, Module] =
    input
      .linesIterator
      .map(Module.fromString)
      .toMap

  trait Module:
    val name: String

    val destinationModules: Vector[String]

    def pulse(sender: String, pulse: Int): (Module, Vector[(String, String, Int)])

  object Module:
    def fromString(s: String): (String, Module) =
      s match
        case s"broadcaster -> $d" => "broadcaster" -> Broadcast("broadcaster", d.split(",").map(_.trim).toVector)
        case s"%$n -> $d"         => n -> FlipFlop(n, d.split(",").map(_.trim).toVector, 0)
        case s"&$n -> $d"         => n -> Conjunction(n, d.split(",").map(_.trim).toVector, d.split(",").map(_.trim -> 0).toMap)

  case class FlipFlop(name: String, destinationModules: Vector[String], state: Int) extends Module:
    def pulse(from: String, pulse: Int): (Module, Vector[(String, String, Int)]) =
      if pulse == 1 then (this, Vector.empty[(String, String, Int)])
      else (copy(state = 1 - state), destinationModules.map((name, _, 1 - state)))

  case class Conjunction(name: String, destinationModules: Vector[String], state: Map[String, Int]) extends Module:
    def pulse(from: String, pulse: Int): (Module, Vector[(String, String, Int)]) =
      val newState = state.updated(from, pulse)

      (copy(state = newState), destinationModules.map((name, _, if newState.values.forall(_ == 1) then 0 else 1)))

  case class Broadcast(name: String, destinationModules: Vector[String]) extends Module:
    def pulse(from: String, pulse: Int): (Module, Vector[(String, String, Int)]) =
      (this, destinationModules.map((name, _, pulse)))

  case class ButtonModule(state: Map[String, Module], low: Int = 0, high: Int = 0):
    val broadcaster: Module =
      state.find((n: String, _) => n == "broadcaster").get._2

    def countPulse(pulse: Int): ButtonModule =
      if pulse == 0 then copy(low = low + 1)
      else copy(high = high + 1)

  object Aptly:
    def push(buttonModule: ButtonModule): ButtonModule =
      @tailrec def loop(todo: Vector[(String, String, Int)], acc: ButtonModule): ButtonModule =
        todo.headOption match
          case None => acc
          case Some(from, to, pulse) =>
            val (m: Module, t: Vector[(String, String, Int)]) =
              acc.state(to).pulse(from, pulse)

            loop(
              todo.tail ++ t,
              acc.copy(state = acc.state.updated(to, m)).countPulse(pulse)
            )

      loop(buttonModule.broadcaster.pulse("", 0)._2, buttonModule)

  lazy val pt1: Int =
    val buttonModule: Iterator[ButtonModule] =
      Iterator.iterate(ButtonModule(moduleConfiguration))(Aptly.push)

    val ButtonModule(_, low, high) =
      buttonModule
        .drop(1000)
        .next

    low * high

  lazy val pt2: Int =
    ???

  answer(1)(pt1)

  answer(2)(pt2)
