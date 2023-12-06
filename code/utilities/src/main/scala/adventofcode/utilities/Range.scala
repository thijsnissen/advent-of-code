package adventofcode
package utilities

import Range.*

case class Range(min: Long, max: Long):
  def intersect(that: Range): Option[Range] =
    val maxmin = min max that.min
    val minmax = max min that.max

    make(maxmin, minmax)

  def diff(that: Range): Set[Range] =
    this.intersect(that) match
      case None => Set(this)
      case Some(overlap) => Set(
          make(this.min, overlap.min - 1),
          make(overlap.max + 1, this.max)
        ).flatten

  def union(that: Range): Range =
    Range(this.min min that.min, this.max max that.max)

  def contains(i: Long): Boolean =
    i >= min && i <= max

  def toVector: Vector[Long] =
    Vector.range(min, max + 1)

object Range:
  def make(min: Long, max: Long): Option[Range] =
    Option.when(min <= max)(min to max)

  extension (min: Long)
    def until(max: Long): Range =
      Range(min, max - 1)

    def to(max: Long): Range =
      Range(min, max)
