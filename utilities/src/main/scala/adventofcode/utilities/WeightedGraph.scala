package adventofcode
package utilities

opaque type WeightedGraph[A] =
  A => Map[A, Int]

object WeightedGraph:
  def unit[A](f: A => Map[A, Int]): WeightedGraph[A] =
    f

  def fromTupleList[A](l: List[(A, A, Int)]): WeightedGraph[A] =
    a =>
      l.view
        .filter((from, _, _) => from == a)
        .map((_, to, edge) => (to, edge))
        .toMap

  extension [A](self: WeightedGraph[A])
    def run(a: A): Map[A, Int] =
      self(a)
