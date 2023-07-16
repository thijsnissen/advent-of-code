import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Test"):
		assertResult(6)(Pos(5, 3).manhattan(Pos(1, 1)))
		assertResult(6)(Pos(-5, -3).manhattan(Pos(1, 1)))
		assertResult(6)(Pos(-5, 3).manhattan(Pos(1, 1)))
		assertResult(6)(Pos(5, -3).manhattan(Pos(1, 1)))
		assertResult(6)(Pos(1, 1).manhattan(Pos(5, 3)))
		assertResult(6)(Pos(1, 1).manhattan(Pos(-5, -3)))
		assertResult(6)(Pos(1, 1).manhattan(Pos(-5, 3)))
		assertResult(6)(Pos(1, 1).manhattan(Pos(5, -3)))
		assertResult(0)(Pos(-10, 5).adjacentHrVr(Box(Pos(0, 0), Pos(10, 10))).size)

		val graph =
			List((1, 2), (1, 5), (1, 9), (2, 3), (3, 4), (5, 6), (5, 8), (6, 7), (9, 10))

		pprint.log(Graph.fromTupleList(graph).breadthFirstSearch(1)(_ == 10))
		pprint.log(Graph.fromTupleList(graph).depthFirstSearch(1)(_ == 10))

		assertResult(List(1, 2, 5, 9, 3, 6, 8, 10))(Graph.fromTupleList(graph).breadthFirstSearch(1)(_ == 10).getOrElse("none"))
		assertResult(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(Graph.fromTupleList(graph).depthFirstSearch(1)(_ == 10).getOrElse("none"))
		assertResult("none")(Graph.fromTupleList(graph).breadthFirstSearch(1)(_ == 11).getOrElse("none"))
		assertResult("none")(Graph.fromTupleList(graph).depthFirstSearch(1)(_ == 11).getOrElse("none"))

		val weightedGraph = List(
			('a', 'b', 2),
			('a', 'c', 1),
			('b', 'd', 1),
			('b', 'e', 2),
			('c', 'd', 3),
			('d', 'e', 4),
			('d', 'f', 5),
			('e', 'f', 2),
			('e', 'g', 5),
			('f', 'g', 10)
		)

		pprint.log(WeightedGraph.fromTupleList(weightedGraph).dijkstraShortestPath('x'))
		pprint.log(WeightedGraph.fromTupleList(weightedGraph).dijkstraShortestPath('a'))

		assertResult(Some((List('a', 'b', 'e', 'f'), 6)))(WeightedGraph.fromTupleList(weightedGraph).shortestPathTo('a')(_ == 'f'))
		assertResult(None)(WeightedGraph.fromTupleList(weightedGraph).shortestPathTo('x')(_ == 'f'))
		assertResult(None)(WeightedGraph.fromTupleList(weightedGraph).shortestPathTo('a')(_ == 'x'))
		assertResult(Some((List('b'), 0)))(WeightedGraph.fromTupleList(weightedGraph).shortestPathTo('b')(_ == 'b'))

		def tree(depth: Int): WeightedGraph[List[Boolean]] =
			WeightedGraph.unit:
				case x if x.size < depth => Map((true :: x) -> 1, (false :: x) -> 2)
				case x if x.size == depth => Map(Nil -> 1)
				case _ => Map.empty

		// pprint.pprintln(tree(5).dijkstraShortestPath(List(true)), height = 1000000)
		// pprint.pprintln(tree(14).shortestPathTo(List(true))(_ == Nil), height = 1000000) // 40 seconden
