package adventofcode
package utilities

object Dijkstra:
	given [A](using ordA: Ordering[A], ordInt: Ordering[Int]): Ordering[(A, Int)] with
		override def compare(x: (A, Int), y: (A, Int)): Int =
			val (xA, xInt) = x
			val (yA, yInt) = y

			if xInt != yInt then
				ordInt.compare(xInt, yInt)
			else
				ordA.compare(xA, yA)

	extension[A] (self: WeightedGraph[A])
		def shortestPathTree(source: A)(using Ordering[A]): (Map[A, Int], Map[A, A]) =
			@annotation.tailrec
			def loop(
				toVisit: Map[A, Int],
				visited: Map[A, Int],
				tree: Map[A, A]
			): (Map[A, Int], Map[A, A]) =
				if toVisit.isEmpty then
					(visited, tree)
				else
					val (vertex: A, edge: Int) =
						toVisit.min

					val toVisitNext: Map[A, Int] =
						for
							(v, e) <- self.run(vertex)

							if ! visited.contains(v) || toVisit.exists((v1, e1) => v1 == v && e < e1)
						yield
							v -> (edge + e)

					val toVisitNextTree: Map[A, A] =
						toVisitNext
							.map((v, _) => v -> vertex)

					loop(toVisit.removed(vertex) ++ toVisitNext, visited + (vertex -> edge), tree ++ toVisitNextTree)

			if self.run(source).isEmpty then
				(Map.empty[A, Int], Map.empty[A, A])
			else
				loop(Map(source -> 0), Map.empty[A, Int], Map.empty[A, A])
