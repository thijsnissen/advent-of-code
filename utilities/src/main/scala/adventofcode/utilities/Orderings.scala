package adventofcode
package utilities

object Orderings:
	given tupleAInt[A] (using ordA: Ordering[A], ordInt: Ordering[Int]): Ordering[(A, Int)] with
		override def compare(a: (A, Int), b: (A, Int)): Int =
			val (aA, aInt) = a
			val (bA, bInt) = b

			if aInt != bInt then
				ordInt.compare(aInt, bInt)
			else
				ordA.compare(aA, bA)

	given posReadingOrder: Ordering[Pos] =
		Ordering.fromLessThan:
			(a, b) => a.y < b.y || (a.y == b.y && a.x < b.x)
