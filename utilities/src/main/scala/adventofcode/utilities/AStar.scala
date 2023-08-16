package adventofcode
package utilities

object AStar:
	given MyOrder[A](using ordA: Ordering[A], ordInt: Ordering[Int], ordDouble: Ordering[Double]): Ordering[(A, (Int, Double))] with
		override def compare(x: (A, (Int, Double)), y: (A, (Int, Double))): Int =
			val (xA, (xInt, xDouble)) = x
			val (yA, (yInt, yDouble)) = y

			if xDouble != yDouble then
				ordDouble.compare(xDouble, yDouble)
			else if xInt != yInt then
				ordInt.compare(xInt, yInt)
			else
				ordA.compare(xA, yA)

	private def pathFromResult[A](visited: Map[A, Int], tree: Map[A, A])(target: A => Boolean): Option[(Int, List[A])] =
		visited.find((v, _) => target(v)) match
			case Some(v, e) => Some(e, GraphTraversal.treeToPath(v, tree, List.empty[A]))
			case None       => None

	extension[A] (self: WeightedGraph[A])
		def shortestPathTo(source: A, target: A => Boolean)(h: (A, Int) => Double)(using Ordering[A]): Option[(Int, List[A])] =
			@annotation.tailrec
			def loop(
				toVisit: Map[A, (Int, Double)],
				visited: Map[A, Int],
				tree: Map[A, A]
			): Option[(Int, List[A])] =
				if toVisit.isEmpty then
					pathFromResult(visited, tree)(target)
				else
					val (vertex, (edge, _)): (A, (Int, Double)) =
						toVisit.min

					if target(vertex) then
						pathFromResult(visited + (vertex -> edge), tree)(target)
					else
						val toVisitNext: Map[A, (Int, Double)] =
							for
								(v, e) <- self.run(vertex)

								if !visited.contains(v) || toVisit.exists((v1, e1) => v1 == v && h(v, e) < e1._2)
							yield
								v -> ((edge + e) -> h(v, e))

						val toVisitNextTree: Map[A, A] =
							toVisitNext
								.map((v, _) => v -> vertex)

						loop(toVisit.removed(vertex) ++ toVisitNext, visited + (vertex -> edge), tree ++ toVisitNextTree)

			if self.run(source).isEmpty then
				pathFromResult(Map.empty[A, Int], Map.empty[A, A])(target)
			else if target(source) then
				pathFromResult(Map(source -> 0), Map.empty[A, A])(target)
			else
				loop(Map(source -> (0 -> h(source, 0))), Map.empty[A, Int], Map.empty[A, A])
