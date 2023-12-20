package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite
import utilities.Range.*

class RangeSpec extends AnyFunSuite:
  test("Range"):
    val rangeTo     = 1 to 10
    val rangeUntil  = 1 until 10
    val range       = Range(3, 6)
    val vectorTo    = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val vectorUntil = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)

    assertResult(rangeTo.toVector)(vectorTo)
    assertResult(rangeUntil.toVector)(vectorUntil)
    assertResult(Range.make(1, 10))(Some(Range(1, 10)))
    assertResult(Range.make(10, 1))(None)
    assertResult(rangeTo.intersect(range))(Some(range))
    assertResult(rangeTo.diff(range))(Set(Range(1, 2), Range(7, 10)))
    assertResult(range.diff(rangeTo))(Set())
    assertResult((1 to 3) union (7 to 9))(rangeUntil)
    assertResult(range.contains(5))(true)
    assertResult(range.contains(2))(false)
    assertResult(false)(range.isEmpty)
    assertResult(true)(range.nonEmpty)
    assertResult(4)(range.size)
