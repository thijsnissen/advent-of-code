package adventofcode
package utilities

case class Cycle[A](stemLength: Int, cycleLength: Int, head: A, last: A)

object Cycle:
  def find[A, B](f: A => A, x0: A)(g: A => B): Cycle[A] =
    @annotation.tailrec
    def findCycleLength(
      tortoise: A,
      hare: A,
      cycleLength: Int,
      power: Int,
    ): Int =
      if g(tortoise) == g(hare) then
        cycleLength
      else if power == cycleLength then
        findCycleLength(hare, f(hare), 1, power * 2)
      else
        findCycleLength(tortoise, f(hare), cycleLength + 1, power)

    @annotation.tailrec
    def findStemLength(
      tortoise: A,
      hare: A,
      prevHare: A,
      stemLength: Int,
    ): (Int, A, A) =
      if g(tortoise) == g(hare) then
        (stemLength, tortoise, prevHare)
      else
        findStemLength(f(tortoise), f(hare), hare, stemLength + 1)

    val cycleLength =
      findCycleLength(x0, f(x0), 1, 1)

    val hare =
      (0 until cycleLength).foldLeft(x0): (acc, _) =>
        f(acc)

    val (stemLength, head, last) =
      findStemLength(x0, hare, hare, 0)

    Cycle(stemLength, cycleLength, head, last)
