package adventofcode
package aoc2018

import utilities.AdventOfCode

import Day19.Device
import Day19.Instruction
import Day19.Registers
import Day19.Registers.*

object Day21 extends AdventOfCode:
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
				Device(ip.toInt, Registers.empty)

	lazy val pt1 =
		device
			.run(instructions)
			.registers
			.get(0)

	lazy val pt2 =
		???

	answer(1)(pt1)

	answer(2)(pt2)
