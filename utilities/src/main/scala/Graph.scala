import scala.collection.immutable.SortedSet

opaque type Graph[A] =
	A => SortedSet[A]

object Graph:
	def fromTupleList[A](l: List[(A, A)])(using Ordering[A]): Graph[A] =
		a =>
			SortedSet.empty[A] ++ 
				l.filter((from, _) => from == a)
					.map((_, to) => to)

	extension [A](self: Graph[A])
		def breadthFirstSearch(root: A)(f: A => Boolean): Option[List[A]] =
			loop(Vector.empty[A] ++ self(root), List(root), false)(f)

		def depthFirstSearch(root: A)(f: A => Boolean): Option[List[A]] =
			loop(Vector.empty[A] ++ self(root), List(root), true)(f)

		@annotation.tailrec
		private def loop(toVisit: Vector[A], visited: List[A], lifo: Boolean)(f: A => Boolean): Option[List[A]] =
			toVisit match
				case h +: _ if f(h) => Some((h :: visited).reverse)
				case h +: t =>
					val toVisit: Vector[A] =
						if lifo then
							self(h).toVector ++ t
						else
							t ++ self(h)

					loop(toVisit, h :: visited, lifo)(f)
				case _ => None
