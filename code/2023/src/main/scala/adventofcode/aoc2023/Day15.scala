package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day15 extends AdventOfCode(Prod):
  val steps: Vector[String] =
    input
      .split(",")
      .map(_.trim)
      .toVector

  case class Box(lenses: Vector[Lens]):
    def boxFocusingPower(boxIndex: Int): Int =
      lenses
        .zipWithIndex
        .map: (lens: Lens, lensIndex: Int) =>
          lens.focusingPower(boxIndex, lensIndex + 1)
        .sum

    def removeLens(label: String): Box =
      Box(lenses.filterNot(_.label == label))

    def addLens(lens: Lens): Box =
      if lenses.exists(_.label == lens.label) then
        Box(lenses.updated(lenses.indexWhere(_.label == lens.label), lens))
      else
        Box(lenses :+ lens)

  object Box:
    def empty: Box =
      Box(Vector.empty[Lens])

    extension (self: Vector[Box])
      def totalFocusingPower: Int =
        self.zipWithIndex.foldLeft(0):
          case (acc: Int, (b: Box, i: Int)) =>
            acc + b.boxFocusingPower(i)

  case class Lens(label: String, focalLength: Int):
    def focusingPower(boxIndex: Int, lensIndex: Int): Int =
      (1 + boxIndex) * lensIndex * focalLength

  object Lens:
    def HashAlgorithm(s: String): Int =
      s.foldLeft(0): (acc: Int, c: Char) =>
        (acc + c.toInt) * 17 % 256

  override lazy val pt1: Int =
    import Lens.*

    steps
      .foldLeft(Seq.empty[Int]): (acc: Seq[Int], s: String) =>
        acc :+ HashAlgorithm(s)
      .sum

  override lazy val pt2: Int =
    import Box.*
    import Lens.*

    steps
      .foldLeft(Vector.fill(256)(Box.empty)): (acc: Vector[Box], s: String) =>
        s match
          case s"$label-" =>
            val box: Int = HashAlgorithm(label)

            acc.updated(box, acc(box).removeLens(label))
          case s"$label=$focalLength" =>
            val box: Int = HashAlgorithm(label)

            acc.updated(box, acc(box).addLens(Lens(label, focalLength.toInt)))
      .totalFocusingPower
