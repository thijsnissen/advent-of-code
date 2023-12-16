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
