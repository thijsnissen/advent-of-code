opaque type WeightedGraph[A] =
	A => Map[A, Int]

object WeightedGraph:
	def unit[A](f: A => Map[A, Int]): WeightedGraph[A] =
		f

	def fromTupleList[A](l: List[(A, A, Int)]): WeightedGraph[A] =
		a =>
			Map.empty[A, Int] ++
				l.filter((from, _, _) => from == a)
					.map((_, to, edge) => (to, edge))

	given dijkstraToVisitOrdering[A]: Ordering[(A, Int)] =
		Ordering.by(a => a._2)

	extension [A](self: WeightedGraph[A])
		def dijkstraShortestPath(source: A): (Map[A, Int], Map[A, A]) =

			@annotation.tailrec
			def loop(toVisit: Vector[(A, Int)], visited: Map[A, Int], tree: Map[A, A])
							(using Ordering[(A, Int)]): (Map[A, Int], Map[A, A]) =
				if toVisit.isEmpty || self(source).isEmpty then
					(visited, tree)
				else
					val (vertex, edge) =
						toVisit.head

					val neighbors: Map[A, Int] =
						for
							(v, e) <- self(vertex)

							if ! visited.contains(v) &&
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

		def shortestPathTo(source: A)(target: A => Boolean): Option[(List[A], Int)] =
			val (edges, tree) = self.dijkstraShortestPath(source)

			@annotation.tailrec
			def loop(k: A, acc: List[A]): List[A] =
				tree.get(k) match
					case Some(v) => loop(v, k :: acc)
					case None    => k :: acc

			tree.find((k, _) => target(k)).map((k, _) => k) match
				case Some(t)                => Some(loop(t, List.empty[A]), edges.getOrElse(t, 0))
				case None if target(source) => Some((List(source), 0))
				case None                   => None
