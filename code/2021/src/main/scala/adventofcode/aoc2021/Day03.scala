package adventofcode
package aoc2021

import utilities.AdventOfCode.*

object Day03 extends AdventOfCode(Prod):
  val diagnosticReport: Vector[Bits] =
    input
      .linesIterator
      .map(_.toVector.map(_.asDigit))
      .toVector

  type Bits = Vector[Int]

  object Bits:
    def mostCommonBit(bits: Bits): Int =
      if bits.count(_ == 1) < bits.count(_ == 0) then 0 else 1

    def leastCommonBit(bits: Bits): Int =
      if bits.count(_ == 0) > bits.count(_ == 1) then 1 else 0

    @tailrec
    def filterByCriterium(
      bits: Vector[Bits],
      criterium: Bits => Int,
      pos: Int = 0
    ): Bits =
      if bits.length == 1 then
        bits.head
      else
        val transposedBits: Vector[Bits] =
          bits.transpose

        filterByCriterium(
          bits.filter(_.apply(pos) == criterium(transposedBits.apply(pos))),
          criterium,
          pos + 1
        )

    extension (b: Bits)
      def toDecimal: Int =
        Integer.parseInt(b.mkString, 2)

  import Bits.*

  lazy val gammaRate: Int =
    diagnosticReport
      .transpose
      .map(mostCommonBit)
      .toDecimal

  lazy val epsilonRate: Int =
    diagnosticReport
      .transpose
      .map(leastCommonBit)
      .toDecimal

  lazy val oxygenGeneratorRating: Int =
    filterByCriterium(diagnosticReport, mostCommonBit)
      .toDecimal

  lazy val CO2ScrubberRating: Int =
    filterByCriterium(diagnosticReport, leastCommonBit)
      .toDecimal

  lazy val pt1: Int =
    val powerConsumption: Int =
      gammaRate * epsilonRate

    powerConsumption

  lazy val pt2: Int =
    val lifeSupportRating: Int =
      oxygenGeneratorRating * CO2ScrubberRating

    lifeSupportRating

  answer(1)(pt1)

  answer(2)(pt2)
