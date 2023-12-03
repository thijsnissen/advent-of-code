package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class CycleSpec extends AnyFunSuite:
  test("Cycle"):
    val cycle: Int => Int =
      case i if i >= 10 => i % 5 + 5
      case i            => i + 1

    assertResult(Cycle(5, 6, 5, 10))(Cycle.find(cycle, 0)(identity))
