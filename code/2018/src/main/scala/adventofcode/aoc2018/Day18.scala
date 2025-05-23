package adventofcode
package aoc2018

import adventofcode.utilities.AdventOfCode.*
import adventofcode.utilities.Pos

object Day18 extends AdventOfCode(Prod):
  import Acres.*
  import AcreType.*

  val landscape: Acres =
    val result: Iterator[(Pos, AcreType)] =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
      yield Pos(x, y) -> fromChar(c)

    Acres.unit(result.toMap)

  enum AcreType:
    case Open
    case Wooded
    case Lumberyard

  object AcreType:
    def fromChar(c: Char): AcreType =
      c match
        case '.' => Open
        case '|' => Wooded
        case '#' => Lumberyard

  opaque type Acres =
    Map[Pos, AcreType]

  object Acres:
    def empty: Acres =
      Map.empty[Pos, AcreType]

    def unit(m: Map[Pos, AcreType]): Acres =
      m

    extension (self: Acres)
      def changeLandscape(minutes: Int): Acres =
        @annotation.tailrec
        def loop(acres: Acres, mins: Int): Acres =
          if mins <= 0 then
            acres
          else
            val newAcres: Acres =
              acres.foldLeft(acres): (acc, acre) =>
                val (p: Pos, a: AcreType) = acre
                val adj: Vector[AcreType] =
                  p.allOffsets.toVector.collect(acres)

                a match
                  case Open if adj.count(_ == Wooded) >= 3 =>
                    acc.updated(p, Wooded)
                  case Wooded if adj.count(_ == Lumberyard) >= 3 =>
                    acc.updated(p, Lumberyard)
                  case Lumberyard
                      if adj.count(_ == Lumberyard) >= 1 && adj.count(
                        _ == Wooded
                      ) >= 1 =>
                    acc.updated(p, Lumberyard)
                  case Lumberyard => acc.updated(p, Open)
                  case _          => acc

            loop(newAcres, mins - 1)

        loop(self, minutes)

  lazy val pt1: Int =
    val acres: Acres    = landscape.changeLandscape(10)
    val wooded: Int     = acres.count((_, a) => a == Wooded)
    val lumberyard: Int = acres.count((_, a) => a == Lumberyard)

    wooded * lumberyard

  lazy val pt2: Int =
    import utilities.Cycle

    val f: Acres => Acres =
      a => a.changeLandscape(1)

    val g: Acres => Int =
      a =>
        a.count((_, a) => a == Wooded) * a.count((_, a) => a == Lumberyard)

    val cycle: Cycle[Acres] =
      Cycle.find(f, landscape)(g)

    val it =
      Iterator.iterate(cycle.head)(f)

    val cycleSum: IndexedSeq[Int] =
      (0 until cycle.cycleLength).map: _ =>
        g(it.next)

    val index: Int =
      (1000000000 - cycle.stemLength) % cycle.cycleLength

    cycleSum(index)

  answer(1)(pt1)

  answer(2)(pt2)
