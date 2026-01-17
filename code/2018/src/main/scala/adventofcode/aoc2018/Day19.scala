package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day19 extends AdventOfCode(Prod):
  val instructions: Vector[Instruction] =
    input
      .linesIterator
      .toVector
      .collect:
        case s"$i0 $i1 $i2 $i3" =>
          Instruction(i0, i1.toInt, i2.toInt, i3.toInt)

  val device: Device =
    input.linesIterator.next match
      case s"#ip $ip" =>
        Device(ip.toInt, Registers.empty)

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
      assert(
        m.keys == Set(0, 1, 2, 3, 4, 5),
        "The device has six registers (numbered 0 through 5)"
      )

      m.withDefaultValue(0)

    def empty: Registers =
      Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0).withDefaultValue(0)

    extension (self: Registers)
      def run(opcode: String, a: Int, b: Int, c: Int): Registers =
        opcode match
          case "addr" => self.updated(c, self(a) + self(b))
          case "addi" => self.updated(c, self(a) + b)
          case "mulr" => self.updated(c, self(a) * self(b))
          case "muli" => self.updated(c, self(a) * b)
          case "banr" => self.updated(c, self(a) & self(b))
          case "borr" => self.updated(c, self(a) | self(b))
          case "bori" => self.updated(c, self(a) | b)
          case "bani" => self.updated(c, self(a) & b)
          case "setr" => self.updated(c, self(a))
          case "seti" => self.updated(c, a)
          case "gtir" => self.updated(c, if a > self(b) then 1 else 0)
          case "gtri" => self.updated(c, if self(a) > b then 1 else 0)
          case "gtrr" => self.updated(c, if self(a) > self(b) then 1 else 0)
          case "eqir" => self.updated(c, if a == self(b) then 1 else 0)
          case "eqri" => self.updated(c, if self(a) == b then 1 else 0)
          case "eqrr" => self.updated(c, if self(a) == self(b) then 1 else 0)

      def getRegister(n: Int): Int =
        self(n)

  import Registers.*

  lazy val pt1: Int =
    device
      .run(instructions)
      .registers
      .getRegister(0)

  lazy val pt2: Int =
    val divisors = (1 to 10551330)
      .filter(i => 10551330 % i == 0)

    divisors.sum

  answer(1)(pt1)

  answer(2)(pt2)

// the algorithm stores the sum of all numers r5 that
// make up r3 when multiplied by a whole numer in r0.
// it starts with r5 = 1 and then multiplies by 1 ...
// r3 until r5 * r1 >= r3. It then raises r5 by 1 and
// starts again multiplying. So to complete the algorihm
// makes 10551330 * 10551330 = 111.330.564.768.900 loops.
// r0 contains the sum of the dividers of r3.
//
//17-35: r3 = (2 * 2 * 19 * 11) + (4 * 22 + 6) = 930
//17-35: r2 = (27 * 28 + 29) * (30 * 14 * 32)  = 10550400
//17-35: r3 = r2 + r3 = 10551330
//
//    0: goto 17
//    1: r5 = 1
//
//    2: r1 = 1
//    3: r2 = r5 * r1
//  4-8: r2 == r3 ? r0 += r5 : r1++
// 9-12: r1 > r3 ? r5++ : goto 3
//13-16: r5 > r3 ? halt : goto 2
//
//   17: r3 += 2
//   18: r3 *= r3
//   19: r3 *= 19
//   20: r3 *= 11
//   21: r2 += 4
//   22: r2 *= 22
//   23: r2 += 6
//   24: r3 += r2
//   25: goto 26 + r0
//   26: goto 1
//   27: r2  = 27
//   28: r2 *= 28
//   29: r2 += 29
//   30: r2 *= 30
//   31: r2 *= 14
//   32: r2 *= 32
//   33: r3 += r2
//   34: r0  = 0
//   35: goto 1
