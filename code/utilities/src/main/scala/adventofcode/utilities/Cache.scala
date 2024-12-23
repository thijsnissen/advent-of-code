package adventofcode
package utilities

import scala.collection.mutable

object Cache:
  def memoize[A, B](f: A => B): A => B =
    val cache = mutable.Map.empty[A, B]

    (a: A) => cache.getOrElseUpdate(a, f(a))

  def memoize[A, B, C](f: (A, B) => C): (A, B) => C =
    val cache = mutable.Map.empty[(A, B), C]

    (a: A, b: B) => cache.getOrElseUpdate((a, b), f(a, b))

  def memoize[A, B, C, D](f: (A, B, C) => D): (A, B, C) => D =
    val cache = mutable.Map.empty[(A, B, C), D]

    (a: A, b: B, c: C) => cache.getOrElseUpdate((a, b, c), f(a, b, c))
