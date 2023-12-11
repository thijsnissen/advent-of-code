package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day06 extends AdventOfCode(Prod):
  val myInput: Vector[Coordinate] =
    input
      .linesIterator
      .zipWithIndex
      .collect:
        case (s"$x, $y", i) => Coordinate(i, x.toInt, y.toInt)
      .toVector

  case class Coordinate(id: Int, x: Int, y: Int):
    def distanceTo(tx: Int, ty: Int): Int =
      math.abs(x - tx) + math.abs(y - ty)

  lazy val maxX: Int = myInput.map(_.x).max + 1
  lazy val maxY: Int = myInput.map(_.y).max + 1

  lazy val grid1: Vector[Vector[Int]] =
    (0 until maxX).foldLeft(Vector.fill(maxY)(Vector.fill(maxX)(-1))):
      (state, x) =>
        (0 until maxY).foldLeft(state): (state, y) =>
          val point = myInput.groupBy(_.distanceTo(x, y)).minBy(_._1)._2
          if point.size == 1 then
            state.updated(y, state(y).updated(x, point.head.id))
          else
            state

  lazy val infiniteAreas: Set[Int] =
    (grid1.head ++ grid1.last ++ grid1.flatMap(
      x =>
        Vector(x.head, x.last)
    )).toSet

  lazy val LargestNonInfiniteArea: Int =
    grid1
      .flatten
      .groupBy(identity)
      .map((id, area) => (id, area.size))
      .filterNot((id, _) => infiniteAreas.contains(id))
      .maxBy(_._2)
      ._2

  lazy val maxDistance = if getEnv == Test then 32 else 10000

  lazy val grid2: Vector[Vector[Int]] =
    (0 until maxX).foldLeft(Vector.fill(maxY)(Vector.fill(maxX)(-1))):
      (state, x) =>
        (0 until maxY).foldLeft(state): (state, y) =>
          if myInput.map(_.distanceTo(x, y)).sum < maxDistance then
            state.updated(y, state(y).updated(x, 0))
          else
            state

  lazy val sizeOfSafeRegion: Int =
    grid2
      .flatten
      .groupBy(identity)
      .map(x => (x._1, x._2.size))
      .find(_._1 == 0)
      .getOrElse((0, 0))
      ._2

  lazy val pt1: Int =
    LargestNonInfiniteArea

  lazy val pt2: Int =
    sizeOfSafeRegion

  answer(1)(pt1)

  answer(2)(pt2)
