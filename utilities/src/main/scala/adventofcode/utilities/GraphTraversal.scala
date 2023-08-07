package adventofcode
package utilities

object GraphTraversal:
	import scala.collection.immutable.SortedSet

	@annotation.tailrec
	def treeToPath[A](target: A, tree: List[(A, A)], acc: List[A]): List[A] =
		tree.find((node, _) => node == target) match
			case Some(to, from) => treeToPath(from, tree, to :: acc)
			case None => target :: acc

	private def updateToVisit[A](as: SortedSet[A], visited: List[(A, A)], from: A): Vector[(A, A)] =
		as
			.filterNot(visited.map((node, _) => node).contains)
			.toVector
			.map(_ -> from)

	extension[A] (self: Graph[A])
		def breadthFirstSearch(source: A)(target: A => Boolean): Option[List[(A, A)]] =
			loop(Vector.empty[(A, A)] ++ self.run(source).toVector.map(_ -> source), List.empty[(A, A)], false)(target)

		def depthFirstSearch(source: A)(target: A => Boolean): Option[List[(A, A)]] =
			loop(Vector.empty[(A, A)] ++ self.run(source).toVector.map(_ -> source), List(source -> source), true)(target)

		def findPathBreadthFirst(source: A)(target: A => Boolean): Option[List[A]] =
			breadthFirstSearch(source)(target) match
				case Some(tree) =>
					val (node, _) = tree.find((n, _) => target(n)).get

					Some(treeToPath(node, tree, List.empty[A]))
				case None => None

		@annotation.tailrec
		private def loop(toVisit: Vector[(A, A)], visited: List[(A, A)], lifo: Boolean)(target: A => Boolean): Option[List[(A, A)]] =
			toVisit match
				case (visit, from) +: _ if target(visit) =>
					Some(((visit -> from) :: visited).reverse)
				case (visit, from) +: tail if lifo =>
					loop(updateToVisit(self.run(visit), visited, visit) ++ tail, (visit, from) :: visited, lifo)(target)
				case (visit, from) +: tail =>
					loop(tail ++ updateToVisit(self.run(visit), visited, visit), (visit, from) :: visited, lifo)(target)
				case _ => None
