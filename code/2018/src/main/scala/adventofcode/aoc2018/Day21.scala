package adventofcode
package aoc2018

import utilities.AdventOfCode.*

// No test input available today
object Day21 extends AdventOfCode(Prod):
  import scala.util.control.Breaks.*

  def reverseEngineeringInput(registers: Vector[Int]): Vector[Int] =
    @annotation.nowarn("msg=unset local variable")
    var Vector(r0, r1, r2, r3, r4, r5) = registers

    breakable:
      r3 = r1 | 65536
      r1 = 6780005

      while true do
        r2 = r3 & 255
        r1 += r2
        r1 = r1 & 16777215
        r1 *= 65899
        r1 = r1 & 16777215

        if 256 > r3 then
          // changed == to != to halt first time
          if r1 != r0 then
            break
          else
            r3 = r1 | 65536
            r1 = 6780005
        else
          r2 = 0
          r5 = r2 + 1
          r5 *= 256

          while r5 <= r3 do
            r2 += 1
            r5 = r2 + 1
            r5 *= 256

          r3 = r2

    Vector(r0, r1, r2, r3, r4, r5)

  override lazy val pt1: Int =
    val registers: Vector[Int] =
      Iterator
        .iterate(Vector.fill(6)(0))(reverseEngineeringInput)
        .drop(1)
        .next

    registers(1)

  override lazy val pt2: Int =
    utilities.Cycle
      .find(reverseEngineeringInput, Vector.fill(6)(0))(r => r(1))
      .last(1)

//#ip 4
//00: r1  = 123
//01: r1  = r1 & 456
//02-05: r1 == 72 ? r1 = 0 : goto 1
//06: r3  = r1 | 65536
//07: r1  = 6780005
//08: r2  = r3 & 255
//09: r1 += r2
//10: r1  = r1 & 16777215
//11: r1 *= 65899
//12: r1  = r1 & 16777215
//13-16: 256 > r3 ? goto 28 : goto 17
//17: r2  = 0
//18: r5  = r2 + 1
//19: r5 *= 256
//20-23: r5 > r3 ? goto 26 : goto 24
//24: r2 += 1
//25: goto 18
//26: r3  = r2
//27: goto 8
//28-30: r1 == r0 ? halt : goto 6
