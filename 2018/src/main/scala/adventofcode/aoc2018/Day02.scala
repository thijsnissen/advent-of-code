package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day02 extends AdventOfCode(Prod):
  val myInput: Vector[String] =
    input.linesIterator.toVector

  def multipleLetterCount(input: Vector[String], n: Int): Int =
    input
      .map: l =>
        l
          .groupBy(identity)
          .map(x => x._2.length)
          .toVector
      .count(l => l.contains(n))

  def findCommonLetters(input: Vector[String]): Option[String] =
    @annotation.tailrec
    def go(ids: Vector[Vector[(Char, Int)]]): Option[String] =
      if ids.nonEmpty then
        val index = ids.tail.indexWhere(id => id.diff(ids.head).size == 1)

        if index >= 0 then
          Some(ids.tail(index).intersect(ids.head).map(_._1).mkString)
        else
          go(ids.tail)
      else
        None

    go(input.map(_.zipWithIndex.toVector))

  lazy val pt1: Int =
    multipleLetterCount(myInput, 2) * multipleLetterCount(myInput, 3)

  lazy val pt2: String =
    findCommonLetters(myInput).getOrElse("Not Found")

  answer(1)(pt1)

  answer(2)(pt2)
