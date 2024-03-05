package adventofcode
package utilities

import Range.*

case class Range(min: Long, max: Long):
  infix def intersect(that: Range): Option[Range] =
    val maxmin = min max that.min
    val minmax = max min that.max

    make(maxmin, minmax)

  infix def diff(that: Range): Set[Range] =
    this.intersect(that) match
      case None => Set(this)
      case Some(overlap) => Set(
          make(this.min, overlap.min - 1),
          make(overlap.max + 1, this.max)
        ).flatten

  infix def union(that: Range): Range =
    Range(this.min min that.min, this.max max that.max)

  def contains(i: Long): Boolean =
    i >= min && i <= max

  def size: Long =
    max - min + 1

  def isEmpty: Boolean =
    max <= min

  def nonEmpty: Boolean =
    !isEmpty

  def toVector: Vector[Long] =
    Vector.range(min, max + 1)

object Range:
  def make(min: Long, max: Long): Option[Range] =
    Option.when(min <= max)(min to max)

  extension (min: Long)
    infix def until(max: Long): Range =
      Range(min, max - 1)

    infix def to(max: Long): Range =
      Range(min, max)
