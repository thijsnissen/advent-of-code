package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day12 extends AdventOfCode(Prod):
  val myInput: Vector[String] =
    input
      .linesIterator
      .collect:
        case s"initial state: $state" => state
        case s"$pattern => #"         => pattern
      .toVector

  case class Mutation(state: String, rules: Vector[String], generation: Long):
    @annotation.tailrec
    final def mutate(generations: Long): Mutation =
      if generations <= 0 then
        this
      else
        val mutatedState = ("..." + state + "...").sliding(5).foldLeft(""):
          case (acc, s) if rules.contains(s) => acc + '#'
          case (acc, _)                      => acc + '.'

        copy(state = mutatedState, generation = generation + 1).mutate(
          generations - 1
        )

    def sumOfAllPotsWithPlants: Long =
      state
        .zipWithIndex
        .collect:
          case ('#', i) => i - generation
        .sum

  object Mutation:
    def fromInput(input: Vector[String]): Mutation =
      Mutation(input.head, input.tail, 0)

    def empty: Mutation =
      Mutation("", Vector.empty[String], 0)

  lazy val pt1: Long =
    Mutation
      .fromInput(myInput)
      .mutate(20)
      .sumOfAllPotsWithPlants

  lazy val pt2: Long =
    import utilities.Cycle

    val f: Mutation => Mutation =
      m => m.mutate(1)

    val g: Mutation => String =
      m => m.state.dropWhile(_ == '.').reverse.dropWhile(_ == '.').reverse

    val cycle: Mutation =
      Cycle.find(f, Mutation.fromInput(myInput))(g).head

    val generation = cycle.generation
    val sum        = cycle.sumOfAllPotsWithPlants
    val interval   =
      cycle.mutate(1).sumOfAllPotsWithPlants - cycle.sumOfAllPotsWithPlants

    sum + (50000000000L - generation) * interval

  answer(1)(pt1)

  answer(2)(pt2)
