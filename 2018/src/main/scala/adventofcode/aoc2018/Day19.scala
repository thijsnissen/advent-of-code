package adventofcode
package aoc2018

import utilities.AdventOfCode

object Day19 extends AdventOfCode:
	given Mode = Mode.Prod

	val instructions: Vector[Instruction] =
		input
			.toVector
			.collect:
				case s"$i0 $i1 $i2 $i3" =>
					Instruction(i0, i1.toInt, i2.toInt, i3.toInt)

	val device: Device =
		input.next match
			case s"#ip $ip" =>
				Device(ip.toInt, Registers.unit(Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0)))

	case class Instruction(opcode: String, a: Int, b: Int, c: Int)

	case class Device(ip: Int, registers: Registers):
		@annotation.tailrec
		final def run(instructions: Vector[Instruction]): Device =
			instructions.lift(registers(ip)) match
				case Some(instruction) => execute(instruction).run(instructions)
				case None              => this

		def execute(instr: Instruction): Device =
			import Registers.*

			val newRegisters =
				registers.run(instr.opcode, instr.a, instr.b, instr.c)

			copy(registers = newRegisters.updated(ip, newRegisters(ip) + 1))

	opaque type Registers =
		Map[Int, Int]

	object Registers:
		def unit(m: Map[Int, Int]): Registers =
			assert(m.size == 6, "The device has four registers (numbered 0 through 5)")

			m.withDefaultValue(0)

		extension (self: Registers)
			def run(opcode: String, a: Int, b: Int, c: Int): Registers =
				opcode match
					case "addr" => self.updated(c, self(a) + self(b))
					case "addi" => self.updated(c, self(a) + b)
					case "mulr" => self.updated(c, self(a) * self(b))
					case "muli" => self.updated(c, self(a) * b)
					case "banr" => self.updated(c, self(a) & self(b))
					case "bani" => self.updated(c, self(a) | self(b))
					case "borr" => self.updated(c, self(a) | b)
					case "bori" => self.updated(c, self(a) & b)
					case "setr" => self.updated(c, self(a))
					case "seti" => self.updated(c, a)
					case "gtir" => self.updated(c, if a > self(b) then 1 else 0)
					case "gtri" => self.updated(c, if self(a) > b then 1 else 0)
					case "gtrr" => self.updated(c, if self(a) > self(b) then 1 else 0)
					case "eqir" => self.updated(c, if a == self(b) then 1 else 0)
					case "eqri" => self.updated(c, if self(a) == b then 1 else 0)
					case "eqrr" => self.updated(c, if self(a) == self(b) then 1 else 0)

	lazy val pt1 =
		device
			.run(instructions)
			.registers(0)

	lazy val pt2 =
		import utilities.Cycle

		val f: Device => Device =
			d => d.execute(instructions(d.registers(d.ip)))

		val g: Device => Int =
			d => d.registers(d.ip)

		val cycle =
			Cycle.find(f, device.copy(registers = device.registers.updated(0, 1)))(g)

		val divisors =
			(1 to cycle.head.registers(3))
				.filter(i => cycle.head.registers(3) % i == 0)

		divisors.sum

	answer(1)(pt1)

	answer(2)(pt2)
