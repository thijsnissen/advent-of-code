//package adventofcode
//package utilities
//
//case class Range[T](start: T, end: T, step: T, isInclusive: Boolean)
//
//object Range:
//  def inclusive[T](start: T, end: T, step: T): Range[T] =
//    Range(start, end, step, true)
//
//  def exclusive[T](start: T, end: T, step: T): Range[T] =
//    Range(start, end, step, false)
//
//  extension[T](self: Range[T])
//    def merge(that: Range[T]): Range[T] =
//      val min = from max that.from
//      val max = to min that.to
//      Option.when(min <= max)(Range(min, max))
