package adventofcode
package aoc2022

import utilities.AdventOfCode.*
import utilities.Grid

object Day10 extends AdventOfCode(Prod):
  import Instruction.*

  val program: Vector[Instruction] =
    input
      .linesIterator
      .flatMap:
        case s"addx $value" => Vector(Addx(0), Addx(value.toInt))
        case "noop"         => Vector(Noop)
      .toVector

  enum Instruction:
    case Addx(value: Int)
    case Noop

  case class CPU(register: Int, cycle: Int):
    def execute(instruction: Instruction): CPU =
      instruction match
        case Addx(value) => copy(register = register + value, cycle = cycle + 1)
        case Noop        => copy(cycle = cycle + 1)

    def sprite: Set[Int] =
      Set(register - 1, register, register + 1)

  lazy val pt1: Int =
    val result: Vector[CPU] =
      program.scanLeft(CPU(1, 1)): (cpu, instr) =>
        cpu.execute(instr)

    val cycles: Set[Int] =
      Set(20, 60, 100, 140, 180, 220)

    result
      .filter(cpu => cycles.contains(cpu.cycle))
      .map(cpu => cpu.cycle * cpu.register)
      .sum

  lazy val pt2: String =
    val result: Vector[CPU] =
      program.scanLeft(CPU(1, 1)): (cpu, instr) =>
        cpu.execute(instr)

    result
      .zipWithIndex
      .foldLeft(Grid.fill(40, 6)('.')):
        case (acc, (cpu, int)) =>
          val x: Int = int % 40
          val y: Int = int / 40

          if cpu.sprite.contains(x) then
            acc(x, y)('■')
          else
            acc
      .asString

  answer(1)(pt1)

  answer(2)(pt2)
