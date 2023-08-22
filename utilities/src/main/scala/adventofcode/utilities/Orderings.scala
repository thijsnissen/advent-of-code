package adventofcode
package utilities

object Orderings:
	given tupleADouble[A] (using ordA: Ordering[A], ordDouble: Ordering[Double]): Ordering[(A, Double)] with
		override def compare(a: (A, Double), b: (A, Double)): Int =
			val (aA, aDouble) = a
			val (bA, bDouble) = b

			if aDouble != bDouble then
				ordDouble.compare(aDouble, bDouble)
			else
				ordA.compare(aA, bA)

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
