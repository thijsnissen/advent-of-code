package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day04 extends AdventOfCode(Prod):
  val scratchcards: Vector[Scratchcard] =
    input
      .linesIterator
      .filter(_.nonEmpty)
      .map(Scratchcard.fromString)
      .toVector

  case class Scratchcard(id: Int, winning: Vector[Int], values: Vector[Int]):
    def matching: Int =
      winning.intersect(values).length

    def score: Int =
      math.pow(2.0, matching.toDouble - 1).toInt

  object Scratchcard:
    def fromString(s: String): Scratchcard =
      s match
        case s"Card $id: ${winning} | ${values}" => Scratchcard(
            id.trim.toInt,
            winning.grouped(3).map(_.trim.toInt).toVector,
            values.grouped(3).map(_.trim.toInt).toVector
          )

    @tailrec
    def processAll(
      scratchcards: Vector[Scratchcard],
      lookup: Map[Int, Vector[Scratchcard]],
      acc: Int
    ): Int =
      if scratchcards.isEmpty then acc
      else
        val copies: Vector[Scratchcard] =
          lookup(scratchcards.head.id)

        processAll(scratchcards.tail ++ copies, lookup, acc + copies.length)

  lazy val pt1: Int =
    scratchcards
      .map(_.score)
      .sum

  lazy val pt2: Int =
    val lookup: Map[Int, Vector[Scratchcard]] =
      scratchcards
        .map: (card: Scratchcard) =>
          card.id -> scratchcards.slice(card.id, card.id + card.matching)
        .toMap

    Scratchcard.processAll(scratchcards, lookup, scratchcards.length)

  answer(1)(pt1)

  answer(2)(pt2)
