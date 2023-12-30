package adventofcode
package aoc2023

import scala.util.Random
import utilities.AdventOfCode.*

object Day25 extends AdventOfCode(Prod):
  val components: Vector[Edge] =
    input
      .linesIterator
      .flatMap(Edge.fromString)
      .toVector

  type Vertex = Set[String]

  case class Edge(a: String, b: String):
    def vertices: Set[Vertex] = Set(Set(a), Set(b))

  object Edge:
    def fromString(s: String): Set[Edge] =
      s match
        case s"$a: $b" =>
          b.split(" ").map((b: String) => Edge(a, b)).toSet

    given Random = Random(2023_12_25)

    extension (self: Vector[Edge])
      def toVertices: Set[Vertex] =
        self.flatMap(_.vertices).toSet

      def minCut(cardinality: Int)(using
        r: Random
      ): (Set[Vertex], Vector[Edge]) =
        @tailrec def loop(
          vertices: Set[Vertex],
          edges: Vector[Edge],
          i: Int = 1
        ): (Set[Vertex], Vector[Edge]) =
          if i % 50 == 0 then
            println(s"Finding minCut($cardinality). Iteration $i.")

          val (v: Set[Vertex], e: Vector[Edge]) =
            Karger.karger(vertices, r.shuffle(edges))

          if e.size == cardinality then (v, e)
          else loop(vertices, edges, i + 1)

        loop(self.toVertices, self)

  object Karger:
    @tailrec def karger(
      vertices: Set[Vertex],
      edges: Vector[Edge]
    ): (Set[Vertex], Vector[Edge]) =
      if vertices.size <= 2 then (vertices, edges)
      else
        val Edge(a: String, b: String) = edges.head

        val va: Vertex = vertices.find(_.contains(a)).get
        val vb: Vertex = vertices.find(_.contains(b)).get
        val vc: Vertex = va ++ vb

        val nextVertices: Set[Vertex] =
          vertices - va - vb + vc

        val nextEdges: Vector[Edge] = edges.filterNot:
          case Edge(a, b) => vc.contains(a) && vc.contains(b)

        karger(nextVertices, nextEdges)

  lazy val pt1: Int =
    import Edge.*
    import Edge.given

    val (vertices: Set[Vertex], _) = components.minCut(3)

    vertices.head.size * vertices.last.size

  answer(1)(pt1)
