package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Box3D
import utilities.Pos3D

object Day22 extends AdventOfCode(Prod):
  val snapshot: Bricks =
    input
      .linesIterator
      .map(Bricks.fromString)
      .toVector

  type Bricks = Vector[Box3D]

  object Bricks:
    def empty: Bricks =
      Vector.empty[Box3D]

    def fromString(s: String): Box3D =
      s match
        case s"$xMin,$yMin,$zMin~$xMax,$yMax,$zMax" => Box3D(
            Pos3D(xMin.toInt, yMin.toInt, zMin.toInt),
            Pos3D(xMax.toInt, yMax.toInt, zMax.toInt)
          )

    given Ordering[Box3D] = Ordering.by:
      case Box3D(min, _) => min.z

    extension (self: Box3D)
      def overlap(that: Box3D): Boolean = (self.min.x max that.min.x) <=
        (self.max.x min that.max.x) &&
        (self.min.y max that.min.y) <= (self.max.y min that.max.y)

    extension (self: Bricks)
      def fall: Bricks =
        @tailrec def loop(todo: Bricks, acc: Bricks): Bricks =
          todo.headOption match
            case None      => acc
            case Some(box) =>
              val z: Int =
                acc.foldLeft(1): (acc: Int, b: Box3D) =>
                  if b.overlap(box) then acc max b.max.z + 1
                  else acc

              loop(
                todo.tail,
                acc :+ Box3D(
                  box.min.copy(z = z),
                  box.max.copy(z = z + box.max.z - box.min.z)
                )
              )

        loop(self.sorted, Bricks.empty)

      def notSafelyDisintegrated: Set[Box3D] =
        self
          .supportedBy
          .filter((_, s: Set[Box3D]) => s.size == 1)
          .flatMap((_, s: Set[Box3D]) => s)
          .toSet

      def supportedBy: Map[Box3D, Set[Box3D]] =
        self
          .map: (box: Box3D) =>
            box -> self.filter(
              (b: Box3D) =>
                box != b && b.overlap(box) && b.max.z + 1 == box.min.z
            ).toSet
          .toMap

      def isSupporting: Map[Box3D, Set[Box3D]] =
        self
          .map: (box: Box3D) =>
            box -> self.filter(
              (b: Box3D) =>
                box != b && b.overlap(box) && box.max.z + 1 == b.min.z
            ).toSet
          .toMap

      def chainReaction: Int =
        val isSupporting: Map[Box3D, Set[Box3D]] = self.isSupporting
        val supportedBy: Map[Box3D, Set[Box3D]]  = self.supportedBy

        @tailrec def loop(todo: Set[Box3D], acc: Set[Box3D]): Int =
          todo.headOption match
            case None      => acc.size - 1
            case Some(box) =>
              val fallen = acc + box

              loop(
                todo.tail ++ isSupporting(box).filter(
                  x => supportedBy(x).forall(fallen.contains)
                ),
                fallen
              )

        self
          .notSafelyDisintegrated
          .toVector
          .map((b: Box3D) => loop(Set(b), Set.empty[Box3D]))
          .sum

  import Bricks.*

  override lazy val pt1: Int =
    snapshot.size - snapshot.fall.notSafelyDisintegrated.size

  override lazy val pt2: Int =
    snapshot.fall.chainReaction
