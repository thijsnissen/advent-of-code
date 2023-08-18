package adventofcode
package utilities

object AStar:
	private def formatResult[A](visited: Map[A, Int], tree: Map[A, A])(target: A => Boolean): Option[(Int, List[A])] =
		visited.find((v, _) => target(v)) match
			case Some(v, e) => Some(e, GraphTraversal.treeToPath(v, tree, List.empty[A]))
			case None => None

	extension[A] (self: WeightedGraph[A])
		def shortestPathTo(source: A, target: A => Boolean)(h: (A, Int) => Double)(using Ordering[A]): Option[(Int, List[A])] =
			@annotation.tailrec
			def loop(
				fScore: Map[A, Int],
				toVisit: Map[A, Int],
				visited: Map[A, Int],
				tree: Map[A, A]
			): Option[(Int, List[A])] =
					???

				loop(fScore, toVisit, visited, tree)

			loop(Map(source -> h(source, 0)), Map(source -> 0), Map.empty[A, Int], Map.empty[A, A])
