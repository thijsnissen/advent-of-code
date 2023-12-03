package adventofcode
package aoc2023

import scala.collection.immutable.SortedMap
import utilities.AdventOfCode.*
import utilities.Box
import utilities.Orderings.posReadingOrder
import utilities.Pos

object Day03 extends AdventOfCode(Prod):
  val engineSchematic: SortedMap[Pos, Char] =
    val schema =
      for
        (l: String, y: Int) <- input.linesIterator.zipWithIndex
        (s: Char, x: Int)   <- l.zipWithIndex
      yield Pos(x, y) -> s

    SortedMap.empty[Pos, Char].withDefaultValue('.') ++ schema.toMap

  def filterNumbers(engineSchematic: SortedMap[Pos, Char]): Vector[(Box, Int)] =
    engineSchematic
      .foldLeft(Vector(Vector.empty[(Pos, Int)])):
        case (acc, (p: Pos, s: Char)) =>
          if s.isDigit then acc.updated(0, acc(0) :+ (p, s.asDigit))
          else Vector.empty[(Pos, Int)] +: acc
      .filter(_.nonEmpty)
      .map: (v: Vector[(Pos, Int)]) =>
        val (pos: Vector[Pos], int: Vector[Int]) = v.unzip

        (Box.bounding(pos), int.mkString.toInt)

  def partNumbers(engineSchematic: SortedMap[Pos, Char]): Vector[Int] =
    filterNumbers(engineSchematic)
      .map: (box: Box, number: Int) =>
        val symbols: Set[Pos] =
          box.allOffsetsFn: (p: Pos) =>
            !engineSchematic(p).isDigit && engineSchematic(p) != '.'

        if symbols.nonEmpty then number else 0

  def gearRatios(engineSchematic: SortedMap[Pos, Char]): Vector[Int] =
    val numbers: Vector[(Box, Int)] = filterNumbers(engineSchematic)

    engineSchematic
      .filter((_, s: Char) => s == '*')
      .keys
      .map: (gear: Pos) =>
        val adj: Vector[(Box, Int)] =
          numbers.filter((box: Box, _) => box.allOffsets.contains(gear))

        if adj.size == 2 then adj.map((_, number: Int) => number).product else 0
      .toVector

  lazy val pt1: Int =
    partNumbers(engineSchematic).sum

  lazy val pt2: Int =
    gearRatios(engineSchematic).sum

  answer(1)(pt1)

  answer(2)(pt2)
