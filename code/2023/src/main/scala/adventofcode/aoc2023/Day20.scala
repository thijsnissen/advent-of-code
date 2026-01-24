package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Utilities.lcm

object Day20 extends AdventOfCode(Prod):
  val moduleConfiguration: Map[String, Module] =
    val modules: Map[String, Module] =
      input
        .linesIterator
        .map(Module.fromString)
        .toMap

    modules.map: (name: String, module: Module) =>
      module match
        case module: Conjunction =>
          val state: Map[String, Int] =
            modules
              .filter((_, m: Module) => m.dest.contains(name))
              .keys
              .map(_ -> 0)
              .toMap

          name -> module.copy(state = state)
        case _ => name -> module

  case class Message(from: String, to: String, pulse: Int)

  trait Module:
    val name: String
    val dest: List[String]
    def incoming(message: Message): (Module, List[Message])

  object Module:
    def fromString(s: String): (String, Module) =
      s match
        case s"%$name -> $dest" => name -> FlipFlop(
            name,
            dest.split(", ").toList,
            0
          )
        case s"&$name -> $dest" => name -> Conjunction(
            name,
            dest.split(", ").toList,
            Map.empty[String, Int]
          )
        case s"$name -> $dest" => name -> Broadcast(
            name,
            dest.split(", ").toList
          )

  case class FlipFlop(name: String, dest: List[String], state: Int)
      extends Module:
    def incoming(message: Message): (Module, List[Message]) =
      message.pulse match
        case 0 =>
          (copy(state = 1 - state), dest.map(Message(name, _, 1 - state)))
        case 1 => (this, List.empty[Message])

  case class Conjunction(
    name: String,
    dest: List[String],
    state: Map[String, Int]
  ) extends Module:
    def incoming(message: Message): (Module, List[Message]) =
      val nextState: Map[String, Int] =
        state.updated(message.from, message.pulse)
      val nextPulse: Int = if nextState.values.forall(_ == 1) then 0 else 1

      (copy(state = nextState), dest.map(Message(name, _, nextPulse)))

  case class Broadcast(name: String, dest: List[String]) extends Module:
    def incoming(message: Message): (Module, List[Message]) =
      (this, dest.map(Message(name, _, message.pulse)))

  case class ButtonModule(
    state: Map[String, Module],
    firstHighFor: Map[String, Int] = Map.empty[String, Int],
    low: Int = 0,
    high: Int = 0,
    pushed: Int = 0
  ):
    def logPulses(message: Message): ButtonModule =
      message.pulse match
        case 0 => copy(low = low + 1)
        case 1 => copy(high = high + 1)

    def logFirstHighs(messages: List[Message]): ButtonModule =
      val nextFirstHighFor: Map[String, Int] =
        firstHighFor.map: (module: String, i: Int) =>
          if messages.exists(
              (m: Message) => m.from == module && m.pulse == 1
            ) && i <= 0
          then module -> pushed
          else module -> i

      copy(firstHighFor = nextFirstHighFor)

    def push: ButtonModule =
      @tailrec def loop(
        todo: List[Message],
        state: ButtonModule
      ): ButtonModule =
        todo.headOption match
          case None                   => state
          case Some(message: Message) =>
            state.state.get(message.to) match
              case None => loop(todo.tail, state.logPulses(message))
              case Some(module: Module) =>
                val (nextModule: Module, nextTodo: List[Message]) =
                  module.incoming(message)

                val nextState: ButtonModule =
                  state
                    .copy(state = state.state.updated(message.to, nextModule))
                    .logPulses(message)
                    .logFirstHighs(nextTodo)

                loop(todo.tail ::: nextTodo, nextState)

      loop(
        List(Message("buttonModule", "broadcaster", 0)),
        copy(pushed = pushed + 1)
      )

  override lazy val pt1: Int =
    val buttonModule: Iterator[ButtonModule] =
      Iterator.iterate(ButtonModule(moduleConfiguration))(_.push)

    val ButtonModule(_, _, low, high, _) =
      buttonModule
        .drop(1000)
        .next

    low * high

  override lazy val pt2: Long =
    if getEnv == Test then 0
    else
      val firstHighFor: List[String] =
        moduleConfiguration.find((_, m: Module) => m.dest.contains("rx")) match
          case Some(_, module: Conjunction) => module.state.keys.toList
          case _                            => List.empty[String]

      val buttonModule: Iterator[ButtonModule] =
        Iterator.iterate(ButtonModule(
          moduleConfiguration,
          firstHighFor.map(_ -> 0).toMap
        ))(_.push)

      buttonModule
        .dropWhile((bm: ButtonModule) => !bm.firstHighFor.values.forall(_ > 0))
        .next
        .firstHighFor
        .map((_, i: Int) => i.toLong)
        .reduce(_.lcm(_))
