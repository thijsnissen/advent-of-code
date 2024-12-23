package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class CacheSpec extends AnyFunSuite:
  test("Cache"):
    val cache1: Int => String =
      Cache.memoize[Int, String](_.toString)

    assertResult("123")(cache1(123))

    val cache2: (Int, Int) => String =
      Cache.memoize[Int, Int, String]: (a: Int, b: Int) =>
        (a + b).toString

    assertResult("46")(cache2(12, 34))

    val cache3: (Int, Int, Int) => String =
      Cache.memoize[Int, Int, Int, String]: (a: Int, b: Int, c: Int) =>
        (a + b + c).toString

    assertResult("61")(cache3(12, 34, 15))
