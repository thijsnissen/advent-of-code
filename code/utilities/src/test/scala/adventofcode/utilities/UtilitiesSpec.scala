package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class UtilitiesSpec extends AnyFunSuite:
  test("Utilities"):
    import Utilities.*

    val fn: Int => Int     = (a: Int) => a + 10
    val fb: Int => Boolean = (a: Int) => a < 100

    assertResult((522, 1034))(exponentialSearch(fn, 10)(750))
    assertResult(740)(binarySearch(fn, 10, 999)(750))
    assertResult(740)(exponentialBinarySearch(fn, 10)(750))

    assertResult((74, 138))(exponentialSearch(10)(fb))
    assertResult(100)(binarySearch(10, 999)(fb))
    assertResult(100)(exponentialBinarySearch(10)(fb))

    val rotateSeq = Utilities.rotateSeq(Vector(1, 2, 3, 4, 5))
    val cycleSeq  = Utilities.cycleSeq(Vector.range(1, 11))

    assertResult(4)(cycleSeq(3))
    assertResult(4)(cycleSeq(-7))
    assertResult(4)(cycleSeq(33))

    assertResult(Vector(4, 5, 1, 2, 3))(rotateSeq(3))
    assertResult(Vector(4, 5, 1, 2, 3))(rotateSeq(-7))
    assertResult(Vector(4, 5, 1, 2, 3))(rotateSeq(33))

    assertResult(3)(3 +% 10)
    assertResult(3)(-7 +% 10)
    assertResult(3)(33 +% 10)

    assertResult(36)(12.lcm(18))
    assertResult(0)(2.lcm(0))
    assertResult(42)(-6L.lcm(14L))

    assertResult(6)(List(1, 2, 3).sumBy(identity))
    assertResult(6)(List(1, 2, 3).productBy(identity))

    assertResult(24)(picksTheorem(42, 38))

    assertResult(15)(5.triangular)
