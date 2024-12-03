package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day10 extends AdventOfCode(Prod):
  val myInput: Vector[Point] =
    input
      .linesIterator
      .map(Point.fromString)
      .toVector

  type Grid[A] = Vector[Vector[A]]

  object Grid:
    def fill[A](x: Int, y: Int)(a: => A): Grid[A] =
      Vector.fill(y, x)(a)

    def fromPoints(points: Vector[Point]): Grid[Char] =
      val xCor = points.minBy(_.x).x.abs
      val yCor = points.minBy(_.y).y.abs

      val (xArea, yArea) = Point.area(points)

      points.foldLeft(Grid.fill(xArea.toInt + 1, yArea.toInt + 1)('.')):
        case (g, p)
            if (0 to xArea.toInt).contains(
              p.x - xCor
            ) && (0 to yArea.toInt).contains(p.y - yCor) =>
          g.updated(p.y - yCor, g(p.y - yCor).updated(p.x - xCor, '■'))
        case (g, _) => g

    extension [A](grid: Grid[A])
      def asString: String =
        grid.map(row => row.map(_.toString).mkString("\n", " ", "")).mkString

  case class Point(x: Int, y: Int, vx: Int, vy: Int):
    def addVelocity: Point =
      copy(x = x + vx, y = y + vy)

  object Point:
    def fromString(s: String): Point =
      s match
        case s"position=<$x, $y> velocity=<$vx, $vy>" =>
          Point(x.trim.toInt, y.trim.toInt, vx.trim.toInt, vy.trim.toInt)

    def area(input: Vector[Point]): (Long, Long) =
      val xArea: Long = input.maxBy(_.x).x - input.minBy(_.x).x
      val yArea: Long = input.maxBy(_.y).y - input.minBy(_.y).y

      (xArea, yArea)

  def lookForMessageInTheSky(input: Vector[Point]): (Grid[Char], Int) =
    @annotation.tailrec
    def go(input: Vector[Point], prevArea: Long, time: Int): (Grid[Char], Int) =
      val newInput       = input.map(_.addVelocity)
      val (xArea, yArea) = Point.area(newInput)

      if xArea * yArea > prevArea then
        (Grid.fromPoints(input), time)
      else
        go(newInput, xArea * yArea, time + 1)

    go(input, Long.MaxValue, time = 0)

  import Grid.*

  lazy val pt1: String =
    val (message, _): (Grid[Char], Int) = lookForMessageInTheSky(myInput)

    message.asString

  lazy val pt2: Int =
    val (_, time): (Grid[Char], Int) = lookForMessageInTheSky(myInput)

    time

  answer(1)(pt1)

  answer(2)(pt2)
