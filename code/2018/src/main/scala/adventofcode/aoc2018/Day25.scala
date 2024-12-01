package adventofcode
package aoc2018

import adventofcode.utilities.AdventOfCode.*
import adventofcode.utilities.Pos4D

object Day25 extends AdventOfCode(Test):
  val fixedPointsInSpacetime: Vector[Pos4D] =
    input
      .linesIterator
      .map:
        case s"$x,$y,$z,$w" =>
          Pos4D(x.trim.toInt, y.trim.toInt, z.trim.toInt, w.trim.toInt)
      .toVector

  @tailrec def findConstellations(
    todo: Vector[Pos4D],
    visited: Vector[Pos4D],
    points: Vector[Pos4D],
    constellations: Int = 0
  ): Int =
    @tailrec def loop(
      todo: Vector[Pos4D],
      visited: Vector[Pos4D]
    ): Vector[Pos4D] =
      todo.headOption match
        case None =>
          visited
        case Some(point) if !visited.contains(point) =>
          loop(
            todo.tail ++ points.filter(_.manhattan(point) <= 3),
            visited :+ point
          )
        case Some(_) =>
          loop(todo.tail, visited)

    todo.headOption match
      case None =>
        constellations
      case Some(point) if !visited.contains(point) =>
        findConstellations(
          todo.tail,
          visited ++ loop(Vector(point), Vector.empty[Pos4D]),
          points,
          constellations + 1
        )
      case Some(_) =>
        findConstellations(todo.tail, visited, points, constellations)

  lazy val pt1: Int =
    findConstellations(
      fixedPointsInSpacetime,
      Vector.empty[Pos4D],
      fixedPointsInSpacetime
    )

  answer(1)(pt1)
