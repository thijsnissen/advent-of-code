package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class GraphSpec extends AnyFunSuite:
  test("Graph & Graph Traversal"):
    val graphList = List(
      (1, 2),
      (1, 5),
      (1, 9),
      (2, 3),
      (3, 4),
      (3, 2),
      (4, 1),
      (5, 6),
      (5, 8),
      (6, 7),
      (9, 10),
      (10, 9),
      (7, 6),
      (6, 8),
      (8, 5),
      (5, 1),
      (9, 2),
      (8, 3),
    )

    val graph =
      Graph.fromTupleList(graphList)

    import GraphTraversal.*

    assertResult(Vector(
      (1, 1),
      (2, 1),
      (5, 1),
      (9, 1),
      (3, 2),
      (6, 5),
      (8, 5),
      (10, 9),
    ))(graph.breadthFirstSearch(1)(_ == 10).getOrElse("not found"))
    assertResult(Vector(
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 1),
      (6, 5),
      (7, 6),
      (8, 5),
      (9, 1),
      (10, 9),
    ))(graph.depthFirstSearch(1)(_ == 10).getOrElse("not found"))
    assertResult("not found")(
      graph.breadthFirstSearch(1)(_ == 11).getOrElse("not found")
    )
    assertResult("not found")(
      graph.depthFirstSearch(1)(_ == 11).getOrElse("not found")
    )

    assertResult(List(1, 9, 10))(
      graph.breadthFirstSearchPathTo(1)(_ == 10).getOrElse("not found")
    )
    assertResult(List(2))(
      graph.breadthFirstSearchPathTo(2)(_ == 2).getOrElse("not found")
    )
    assertResult(List(4, 1, 5, 8))(
      graph.breadthFirstSearchPathTo(4)(_ == 8).getOrElse("not found")
    )
    assertResult(List(10, 9, 2, 3))(
      graph.breadthFirstSearchPathTo(10)(_ == 3).getOrElse("not found")
    )

  test("Weighted Graph, Dijkstra & AStar"):
    import Dijkstra.*

    val weightedGraph = List(
      ('z', 'y', 10),
      ('z', 'x', 1),
      ('x', 'w', 2),
      ('w', 'y', 1),
    )

    val res =
      (
        Map('z' -> 0, 'x'   -> 1, 'w'   -> 3, 'y' -> 4),
        Map('y' -> 'w', 'x' -> 'z', 'w' -> 'x'),
      )

    assertResult(res)(
      WeightedGraph.fromTupleList(weightedGraph).shortestPathTree('z')
    )

    assertResult((Map('a' -> 0), Map.empty[Char, Char]))(
      WeightedGraph.fromTupleList(weightedGraph).shortestPathTree('a')
    )

    // def tree(depth: Int): WeightedGraph[List[Boolean]] =
    //  WeightedGraph.unit:
    //    case x if x.size < depth  => Map((true :: x) -> 1, (false :: x) -> 2)
    //    case x if x.size == depth => Map(Nil -> 1)
    //    case _                    => Map.empty

    // val testTree = tree(15).shortestPathTree(List(true))

    import AStar.*
    import Orderings.posReadingOrder

    val aStarStart    = Pos(0, 0)
    val aStarTarget   = Pos(4, 4)
    val aStarSource   = Pos(1, 1)
    val aStarGraphBox = Box(aStarStart, aStarTarget)
    val aStarGraph =
      for
        b <- aStarGraphBox.iterator
        a <- b.axisOffsetsFn(aStarGraphBox.contains)
      yield (b, a, 1)

    val heuristic: (Pos, Int) => Double =
      (v, _) => aStarTarget.euclidean(v)

    val aStar = WeightedGraph.fromTupleList(aStarGraph.toList)
    val aStarResult = (
      6,
      List(
        Pos(x = 1, y = 1),
        Pos(x = 2, y = 1),
        Pos(x = 2, y = 2),
        Pos(x = 3, y = 2),
        Pos(x = 3, y = 3),
        Pos(x = 4, y = 3),
        Pos(x = 4, y = 4),
      ),
    )

    assertResult(Some(aStarResult))(aStar.shortestPathTo(
      aStarSource,
      _ == aStarTarget,
    )(heuristic))
    assertResult(Some(0, List(aStarSource)))(aStar.shortestPathTo(
      aStarSource,
      _ == aStarSource,
    )(heuristic))
    assertResult(None)(
      aStar.shortestPathTo(aStarSource, _ == Pos(5, 5))(heuristic)
    )

    val aStarTestBox = Box(Pos(0, 0), Pos(10, 10))
    val aStarTestSeq =
      aStarTestBox.iterator.toVector.filter(p => p.x != 5 || p.y == 10)

    val aStarTestGraph =
      for
        b <- aStarTestSeq
        a <- b.axisOffsetsFn(aStarTestSeq.contains)
      yield (b, a, 1)

    val aStarTestH: (Pos, Int) => Double =
      (v, _) => math.sqrt(math.pow(9 - v.x, 2) + math.pow(v.y - 1, 2))

    val aStarTest = WeightedGraph.fromTupleList(aStarTestGraph.toList)

    val (aStarDist, aStarRes) =
      aStarTest.shortestPathTo(Pos(1, 1), _ == Pos(9, 1))(aStarTestH).get

    val (dijkstraEdges, dijkstraTree) = aStarTest.shortestPathTree(Pos(1, 1))

    // val dijkstraRes = utilities.GraphTraversal.treeToPath(
    //  Pos(9, 1),
    //  dijkstraTree,
    //  List.empty[Pos]
    // )

    // println:
    //	s"AStar: $aStarDist" +
    //		Pos.asString(aStarTestSeq.filterNot(aStarRes.contains))
    //
    // println:
    //	s"Dijkstra: ${dijkstraEdges(Pos(9, 1))}" +
    //		Pos.asString(aStarTestSeq.filterNot(dijkstraRes.contains))

    assertResult(aStarDist)(dijkstraEdges(Pos(9, 1)))
