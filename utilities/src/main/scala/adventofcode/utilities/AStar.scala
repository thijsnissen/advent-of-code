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
				fScore: Map[A, Double],
				toVisit: Map[A, Int],
				visited: Map[A, Int],
				tree: Map[A, A]
			): Option[(Int, List[A])] =
				if toVisit.isEmpty then
					formatResult(visited, tree)(target)
				else
					import Orderings.tupleADouble

					val (vertex, edge): (A, Int) =
						val (v, _): (A, Double) =
							fScore.min

						v -> toVisit(v)

					if target(vertex) then
						formatResult(visited + (vertex -> edge), tree)(target)
					else
						val neighborsToVisit: Map[A, Int] =
							for
								(v, e) <- self.run(vertex)

								if (edge + e) < visited.getOrElse(v, Int.MaxValue)
								if (edge + e) < toVisit.getOrElse(v, Int.MaxValue)
							yield
								v -> (edge + e)

						val neighborsFScore: Map[A, Double] =
							neighborsToVisit.map:
								(v, e) => v -> (e + h(v, e))

						val neighborsTree: Map[A, A] =
							neighborsToVisit.map:
								(v, _) => v -> vertex

						loop(
							fScore.removed(vertex) ++ neighborsFScore,
							toVisit.removed(vertex) ++ neighborsToVisit,
							visited + (vertex -> edge),
							tree ++ neighborsTree
						)

			loop(Map(source -> h(source, 0)), Map(source -> 0), Map.empty[A, Int], Map.empty[A, A])
