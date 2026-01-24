package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day03 extends AdventOfCode(Prod):
  val myInput: Vector[Claim] =
    input
      .linesIterator
      .collect:
        case s"#${id} @ ${left},${top}: ${width}x${height}" =>
          Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
      .toVector

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int):
    val xStart: Int = left
    val xEnd: Int   = left + width - 1
    val yStart: Int = top
    val yEnd: Int   = top + height - 1

    def getSquares: Set[Square] = (yStart to yEnd)
      .flatMap: y =>
        (xStart to xEnd).map: x =>
          Square(x, y, id)
      .toSet

  case class Square(x: Int, y: Int, id: Int)

  def findClaimIDsWithOverlap(input: Vector[Claim]): Set[Int] =
    input
      .flatMap(_.getSquares)
      .groupBy:
        case Square(x, y, _) => (x, y)
      .filter((_, s) => s.size > 1)
      .flatMap(c => c._2)
      .map:
        case Square(_, _, id) => id
      .toSet

  override lazy val pt1: Int =
    myInput
      .flatMap(_.getSquares)
      .groupBy:
        case Square(x, y, _) => (x, y)
      .count((_, s) => s.size > 1)

  override lazy val pt2: Int =
    myInput
      .map(_.id)
      .diff(findClaimIDsWithOverlap(myInput).toVector)
      .head
