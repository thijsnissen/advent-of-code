package adventofcode
package aoc2018

import adventofcode.utilities.AdventOfCode.*
import adventofcode.utilities.Box3D
import adventofcode.utilities.Pos3D
import scala.collection.mutable
import scala.math.*

object Day23 extends AdventOfCode(Prod):
  val nanobots: Set[Nanobot] =
    input
      .linesIterator
      .map(Nanobot.fromString)
      .toSet

  case class Nanobot(pos: Pos3D, signalRadius: Int):
    def nanobotsInRange(nanobots: Set[Nanobot]): Int =
      nanobots.count(_.pos.manhattan(pos) <= signalRadius)

  object Nanobot:
    def fromString(s: String): Nanobot =
      s match
        case s"pos=<$x,$y,$z>, r=$r" =>
          Nanobot(Pos3D(x.toInt, y.toInt, z.toInt), r.toInt)

    extension (self: Box3D)
      def nanobotsInRange(nanobots: Set[Nanobot]): Int =
        nanobots.count: nanobot =>
          self.manhattan(nanobot.pos) <= nanobot.signalRadius

      def toOctants: Set[Box3D] =
        val mid = (self.min + self.max) / 2

        Set(
          Box3D(
            Pos3D(self.min.x, self.min.y, self.min.z),
            Pos3D(mid.x, mid.y, mid.z)
          ),
          Box3D(
            Pos3D(mid.x, self.min.y, self.min.z),
            Pos3D(self.max.x, mid.y, mid.z)
          ),
          Box3D(
            Pos3D(self.min.x, mid.y, self.min.z),
            Pos3D(mid.x, self.max.y, mid.z)
          ),
          Box3D(
            Pos3D(mid.x, mid.y, self.min.z),
            Pos3D(self.max.x, self.max.y, mid.z)
          ),
          Box3D(
            Pos3D(self.min.x, self.min.y, mid.z + 1),
            Pos3D(mid.x, mid.y, self.max.z)
          ),
          Box3D(
            Pos3D(mid.x, self.min.y, mid.z),
            Pos3D(self.max.x, mid.y, self.max.z)
          ),
          Box3D(
            Pos3D(self.min.x, mid.y, mid.z),
            Pos3D(mid.x, self.max.y, self.max.z)
          ),
          Box3D(
            Pos3D(mid.x, mid.y, mid.z),
            Pos3D(self.max.x, self.max.y, self.max.z)
          )
        )

    def mostInRangeAtPos(nanobots: Set[Nanobot])(closestTo: Pos3D): Pos3D =
      val box =
        Box3D.bounding(nanobots.map(_.pos))

      val queue =
        mutable.PriorityQueue((box, box.nanobotsInRange(nanobots))):
          Ordering.by((_, nanobotsInRange) => nanobotsInRange)

      @tailrec def loop(acc: Map[Pos3D, Int], visited: Set[Box3D]): Set[Pos3D] =
        queue.dequeue match
          case (box, _) if visited.contains(box) =>
            loop(acc, visited)
          case (_, inRange) if acc.values.exists(_ > inRange) =>
            acc.keys.toSet
          case (box, inRange) if box.min == box.max =>
            loop(acc.updated(box.min, inRange), visited + box)
          case (box, _) =>
            queue ++= box.toOctants.map(b => b -> b.nanobotsInRange(nanobots))

            loop(acc, visited + box)

      loop(Map.empty[Pos3D, Int], Set.empty[Box3D])
        .minBy(_.manhattan(closestTo))

  lazy val pt1: Int =
    nanobots
      .maxBy(_.signalRadius)
      .nanobotsInRange(nanobots)

  lazy val pt2: Long =
    Nanobot
      .mostInRangeAtPos(nanobots)(closestTo = Pos3D.zero)
      .manhattan(Pos3D.zero)

  answer(1)(pt1)

  answer(2)(pt2)
