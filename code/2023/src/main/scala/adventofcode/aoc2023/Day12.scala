package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day12 extends AdventOfCode(Test):
  val conditionRecords: Vector[Record] =
    input
      .linesIterator
      .map(Record.fromString)
      .toVector

  case class Record(springs: String, groups: List[Long]):
    def unfold: Record =
      Record(
        List.fill(5)(springs).mkString("?"),
        List.fill(5)(groups).flatten
      )

    def matched(s: String): Boolean =
      s.split('.').filter(_.nonEmpty).map(_.length).toList == groups

    def arrangements: List[String] =
      @tailrec def loop(todo: List[String], acc: List[String]): List[String] =
        if todo.isEmpty then
          acc
        else if todo.head.length == springs.length then
          loop(todo.tail, if matched(todo.head) then todo.head :: acc else acc)
        else
          springs(todo.head.length) match
            case '?' => loop(todo.head + '.' :: todo.head + '#' :: todo.tail, acc)
            case c   => loop(todo.head + c :: todo.tail, acc)

      loop(List(""), List.empty[String])

  object Record:
    def fromString(s: String): Record =
      val Array(springs, groups) = s.split(" ")

      Record(springs, groups.split(",").map(_.toLong).toList)

    extension (self: Vector[Record])
      def sumArrangements: Long =
        self
          .flatMap(_.arrangements)
          .length

  lazy val pt1: Long =
    conditionRecords.sumArrangements

  lazy val pt2: Long =
    // conditionRecords
    //   .map(_.unfold)
    //   .sumArrangements
    ???

  answer(1)(pt1)

  answer(2)(pt2)
