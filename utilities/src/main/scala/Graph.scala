import scala.collection.immutable.SortedSet

opaque type Graph[A] =
	A => SortedSet[A]

object Graph:
	def unit[A](f: A => SortedSet[A]): Graph[A] =
		f

	def fromTupleList[A](l: List[(A, A)])(using Ordering[A]): Graph[A] =
		a =>
			SortedSet.empty[A] ++
				l.view
					.filter((from, _) => from == a)
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
				case h +: t if lifo => loop(self(h).toVector ++ t, updateVisited(h, visited), lifo)(f)
				case h +: t         => loop(t ++ self(h), updateVisited(h, visited), lifo)(f)
				case _              => None

		private def updateVisited(a: A, visited: List[A]): List[A] =
			if visited.contains(a) then
				visited
			else
				a :: visited
