package adventofcode
package aoc2023

import scala.collection.immutable.SortedMap
import utilities.AdventOfCode.*
import utilities.Box
import utilities.Orderings.posReadingOrder
import utilities.Pos

object Day03 extends AdventOfCode(Prod):
  val engineSchematic: SortedMap[Pos, Char] =
    val visual: Iterator[(Pos, Char)] =
      for
        (line: String, y: Int) <- input.linesIterator.zipWithIndex
        (symbol: Char, x: Int) <- line.zipWithIndex
      yield Pos(x, y) -> symbol

    SortedMap.empty[Pos, Char].withDefaultValue('.') ++ visual.toMap

  def filterNumbers(engineSchematic: SortedMap[Pos, Char]): Vector[(Box, Int)] =
    engineSchematic
      .foldLeft(Vector(Vector.empty[(Pos, Int)])):
        case (acc, (pos: Pos, symbol: Char)) =>
          if symbol.isDigit then acc.updated(0, acc(0) :+ (pos, symbol.asDigit))
          else Vector.empty[(Pos, Int)] +: acc
      .filter(_.nonEmpty)
      .map: (found: Vector[(Pos, Int)]) =>
        val (numbers: Vector[Pos], number: Vector[Int]) = found.unzip

        (Box.bounding(numbers), number.mkString.toInt)

  def partNumbers(engineSchematic: SortedMap[Pos, Char]): Vector[Int] =
    filterNumbers(engineSchematic)
      .map: (numbers: Box, number: Int) =>
        val symbols: Set[Pos] =
          numbers.allOffsetsFn: (pos: Pos) =>
            !engineSchematic(pos).isDigit && engineSchematic(pos) != '.'

        if symbols.nonEmpty then number else 0

  def gearRatios(engineSchematic: SortedMap[Pos, Char]): Vector[Int] =
    val numbers: Vector[(Box, Int)] = filterNumbers(engineSchematic)

    engineSchematic
      .filter((_, symbol: Char) => symbol == '*')
      .keys
      .map: (gear: Pos) =>
        val adjNumbers: Vector[(Box, Int)] =
          numbers.filter((numbers: Box, _) => numbers.allOffsets.contains(gear))

        if adjNumbers.size == 2 then
          adjNumbers.map((_, number: Int) => number).product
        else 0
      .toVector

  lazy val pt1: Int =
    partNumbers(engineSchematic).sum

  lazy val pt2: Int =
    gearRatios(engineSchematic).sum

  answer(1)(pt1)

  answer(2)(pt2)
