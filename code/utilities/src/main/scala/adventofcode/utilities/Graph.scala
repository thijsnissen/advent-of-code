package adventofcode
package utilities

import scala.collection.immutable.SortedSet

opaque type Graph[A] =
  A => SortedSet[A]

object Graph:
  def unit[A](f: A => SortedSet[A]): Graph[A] =
    f

  def fromTupleList[A](l: List[(A, A)])(using Ordering[A]): Graph[A] =
    a =>
      SortedSet.empty[A] ++
        l.view
          .filter((from, _) => from == a)
          .map((_, to) => to)

  extension [A](self: Graph[A])
    def run(a: A): SortedSet[A] =
      self(a)
