object Utilities extends App:
	import scala.collection.immutable.SortedMap
	import scala.collection.immutable.SortedSet

	case class Graph[A](get: SortedMap[A, SortedSet[A]])

	object Graph:
		def fromTupleList[A](list: List[(A, A)])(using Ordering[A]): Graph[A] =
			val graph = list
				.groupBy((from, _) => from)
				.map((from, lto) => (from, SortedSet.empty[A] ++ lto.map((_, to) => to).toSet))

			Graph(SortedMap.empty[A, SortedSet[A]] ++ graph)

	// The Breadth First Search (BFS) algorithm is used to search a graph or tree data
	// structure for a node that meets a set of criteria. It visits all nodes at the
	// current depth level before moving on to the nodes at the next depth level.

	import scala.collection.immutable.Queue

	def BreadthFirstSearch[A](graph: Graph[A], root: A)(f: A => Boolean): Option[List[A]] =
		@annotation.tailrec
		def go(queue: Queue[A], visited: List[A]): Option[List[A]] =
			queue.dequeueOption match
				case Some(head, _) if f(head) => Some((head :: visited).reverse)
				case Some(head, tail)         => go(tail ++ graph.get.getOrElse(head, Nil), head :: visited)
				case None                     => None

		go(Queue.empty[A] ++ graph.get(root), List(root))

	// The Depth First Search (DFS) algorithm is used to search a graph or tree data
	// structure for a node that meets a set of criteria. It explores as far as possible
	// along each branch before backtracking.

	def DepthFirstSearch[A](graph: Graph[A], root: A)(f: A => Boolean): Option[List[A]] =
		@annotation.tailrec
		def go(stack: List[A], visited: List[A]): Option[List[A]] =
			stack match
				case head :: _ if f(head) => Some((head :: visited).reverse)
				case head :: tail         => go((graph.get.getOrElse(head, Nil) ++ tail).toList, head :: visited)
				case Nil                  => None

		go(graph.get(root).toList, List(root))

	// Tests
	val graph =
		List((1, 2), (1, 5), (1, 9), (2, 3), (3, 4), (5, 6), (5, 8), (6, 7), (9, 10))

	pprint.log(BreadthFirstSearch(Graph.fromTupleList(graph), 1)(_ == 10))
	pprint.log(DepthFirstSearch(Graph.fromTupleList(graph), 1)(_ == 10))

	assert(BreadthFirstSearch(Graph.fromTupleList(graph), 1)(_ == 10).get == List(1, 2, 5, 9, 3, 6, 8, 10))
	assert(DepthFirstSearch(Graph.fromTupleList(graph), 1)(_ == 10).get == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
	assert(BreadthFirstSearch(Graph.fromTupleList(graph), 1)(_ == 11).getOrElse("none") == "none")
	assert(DepthFirstSearch(Graph.fromTupleList(graph), 1)(_ == 11).getOrElse("none") == "none")
