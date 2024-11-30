package adventofcode
package utilities

import scala.math

object Utilities:
  import scala.math.Integral.Implicits.*
  import scala.math.Ordering.Implicits.*

  extension [A](a: A)
    def dump: A =
      pprint.log(a)

      sys.exit(1)

  def exponentialSearch[A, B](f: A => B, min: A)(x: B)(using
    int: Integral[A],
    ord: Ordering[B]
  ): (A, A) =
    @annotation.tailrec
    def loop(lower: A, upper: A): (A, A) =
      if f(min + upper) >= x then
        (min + lower, min + upper)
      else
        loop(upper, upper * int.fromInt(2))

    if f(min) < x then
      loop(int.zero, int.one)
    else
      (min, min + int.one)

  def binarySearch[A, B](f: A => B, min: A, max: A)(x: B)(using
    int: Integral[A],
    ord: Ordering[B]
  ): A =
    @annotation.tailrec
    def loop(lower: A, upper: A): A =
      if lower == upper then
        lower
      else
        val middle = (upper - lower) / int.fromInt(2) + lower

        if f(middle) >= x then
          loop(lower, middle)
        else
          loop(middle + int.one, upper)

    loop(min, max)

  def exponentialBinarySearch[A, B](f: A => B, min: A)(x: B)(using
    Integral[A],
    Ordering[B]
  ): A =
    val (lower, upper) =
      exponentialSearch(f, min)(x)

    binarySearch(f, lower, upper)(x)

  def cycleSeq[A](sa: Seq[A])(i: Int): A =
    sa(i +% sa.length)

  def rotateSeq[A](sa: Seq[A])(i: Int): Seq[A] =
    val (init, tail) =
      sa.splitAt(i +% sa.length)

    tail ++ init

  def picksTheorem(area: Long, boundaryPoints: Long): Long =
    area - boundaryPoints / 2 + 1

  extension [A: Integral](a: A)
    @targetName("wholeNumberModulo")
    def +%(n: A): A = (a % n + n) % n

    @tailrec
    def gcd(b: A): A =
      if b == Integral[A].zero then a.abs else b.gcd(a % b)

    def lcm(b: A): A =
      (a * b).abs / a.gcd(b)

  extension [A](i: Iterable[A])
    def sumBy[B: Integral](f: A => B): B =
      i.foldLeft(Integral[B].zero): (acc, elem) =>
        Integral[B].plus(acc, f(elem))

    def productBy[B: Integral](f: A => B): B =
      i.foldLeft(Integral[B].one): (acc, elem) =>
        Integral[B].times(acc, f(elem))
