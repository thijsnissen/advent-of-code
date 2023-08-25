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
				if toVisit.isEmpty then
					(visited, tree)
				else
					import Orderings.tupleAInt

					val (vertex, edge): (A, Int) =
						toVisit.min

					val neighborsToVisit: Map[A, Int] =
						for
							(v, e) <- self.run(vertex)

							if (edge + e) < visited.getOrElse(v, Int.MaxValue)
							if (edge + e) < toVisit.getOrElse(v, Int.MaxValue)
						yield
							v -> (edge + e)

					val neighborsTree: Map[A, A] =
						neighborsToVisit.map:
							(v, _) => v -> vertex

					loop(toVisit.removed(vertex) ++ neighborsToVisit, visited + (vertex -> edge), tree ++ neighborsTree)

			loop(Map(source -> 0), Map.empty[A, Int], Map.empty[A, A])
