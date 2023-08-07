package adventofcode
package utilities

// TODO: Implement Dijkstra using a faster data structure, for example scala.mutable.PriorityQueue

object Dijkstra:
	given[A] (using ordA: Ordering[A], ordInt: Ordering[Int]): Ordering[(A, Int)] with
		override def compare(x: (A, Int), y: (A, Int)): Int =
			if x._2 == y._2 then
				ordA.compare(x._1, y._1)
			else
				ordInt.compare(x._2, y._2)

	extension[A] (self: WeightedGraph[A])
		def dijkstraShortestPath(source: A)(using Ordering[A]): (Map[A, Int], Map[A, A]) =
			@annotation.tailrec
			def loop(toVisit: Vector[(A, Int)], visited: Map[A, Int], tree: Map[A, A])
							(using Ordering[(A, Int)]): (Map[A, Int], Map[A, A]) =
				if toVisit.isEmpty || self.run(source).isEmpty then
					(visited, tree)
				else
					val (vertex, edge) =
						toVisit.head

					val neighbors: Map[A, Int] =
						for
							(v, e) <- self.run(vertex)

							if !visited.contains(v) &&
								edge + e < toVisit
									.find((v1, _) => v == v1)
									.map((_, e1) => e1)
									.getOrElse(Int.MaxValue)
						yield
							v -> (edge + e)

					loop(
						(toVisit.tail ++ neighbors)
							.groupMapReduce((v, _) => v)((_, e) => e)(_ min _)
							.toVector
							.sorted,
						visited + (vertex -> edge),
						tree ++ neighbors.map((v, _) => (v, vertex))
					)

			loop(Vector((source, 0)), Map.empty[A, Int], Map.empty[A, A])

		def shortestPathTo(source: A)(target: A => Boolean)(using Ordering[A]): Option[(List[A], Int)] =
			val (edges, tree) = self.dijkstraShortestPath(source)

			@annotation.tailrec
			def loop(k: A, acc: List[A]): List[A] =
				tree.get(k) match
					case Some(v) => loop(v, k :: acc)
					case None => k :: acc

			edges.toVector.sorted.find((k, _) => target(k)) match
				case Some(t, _) => Some(loop(t, List.empty[A]), edges.getOrElse(t, 0))
				case None if target(source) => Some((List(source), 0))
				case None => None