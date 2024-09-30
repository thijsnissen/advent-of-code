package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Pos

object Day11 extends AdventOfCode(Prod):
  val image: Vector[Pos] =
    (for
      (l, y) <- input.linesIterator.zipWithIndex
      (c, x) <- l.zipWithIndex
      if c != '.'
    yield Pos(x, y)).toVector

  def offsetPos(pos: Pos, image: Vector[Pos], expansion: Int): Pos =
    val xEmpty: Int = pos.x - image.distinctBy(_.x).count(_.x < pos.x)
    val yEmpty: Int = pos.y - image.distinctBy(_.y).count(_.y < pos.y)

    pos + Pos(xEmpty * (expansion - 1), yEmpty * (expansion - 1))

  def shortestPaths(image: Vector[Pos], expansion: Int): Vector[Long] =
    image
      .map(offsetPos(_, image, expansion))
      .combinations(2)
      .collect:
        case Vector(a: Pos, b: Pos) => a.manhattan(b)
      .toVector

  lazy val pt1: Long =
    shortestPaths(image, 2).sum

  lazy val pt2: Long =
    shortestPaths(image, 1_000_000).sum

  answer(1)(pt1)

  answer(2)(pt2)
