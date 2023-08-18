package adventofcode
package utilities

object Dijkstra:
	extension[A] (self: WeightedGraph[A])
		def shortestPathTree(source: A)(using Ordering[A]): (Map[A, Int], Map[A, A]) =
			@annotation.tailrec
			def loop(
				toVisit: Map[A, Int],
				visited: Map[A, Int],
				tree: Map[A, A]
			): (Map[A, Int], Map[A, A]) =
				???

				loop(toVisit, visited, tree)

			if self.run(source).isEmpty then
				(Map.empty[A, Int], Map.empty[A, A])
			else
				loop(Map(source -> 0), Map.empty[A, Int], Map.empty[A, A])
