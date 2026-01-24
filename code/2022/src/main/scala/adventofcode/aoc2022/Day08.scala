package adventofcode
package aoc2022

import utilities.AdventOfCode.*
import utilities.Grid
import utilities.Pos

object Day08 extends AdventOfCode(Prod):
  val trees: Grid[Tree] =
    Grid.unit:
      input
        .linesIterator
        .map(_.toVector)
        .zipWithIndex
        .map: (l, y) =>
          l.zipWithIndex.map: (c, x) =>
            Tree(Pos(x, y), c.asDigit)
        .toVector

  case class Tree(pos: Pos, height: Int):
    def isVisible(trees: Grid[Tree]): Boolean =
      val (left, right) =
        trees.getRow(pos.y).filterNot(_.pos == pos).splitAt(pos.x)
      val (up, down) =
        trees.getCol(pos.x).filterNot(_.pos == pos).splitAt(pos.y)

      left.length == left.reverse.takeWhile(_.height < height).length ||
      right.length == right.takeWhile(_.height < height).length ||
      up.length == up.reverse.takeWhile(_.height < height).length ||
      down.length == down.takeWhile(_.height < height).length

    def scenicScore(trees: Grid[Tree]): Int =
      val (left, right) =
        trees.getRow(pos.y).filterNot(_.pos == pos).splitAt(pos.x)
      val (up, down) =
        trees.getCol(pos.x).filterNot(_.pos == pos).splitAt(pos.y)

      (up.length min (up.reverse.takeWhile(_.height < height).length + 1)) *
        (left.length min
          (left.reverse.takeWhile(
            _.height < height
          ).length + 1)) *
        (down.length min (down.takeWhile(_.height < height).length + 1)) *
        (right.length min (right.takeWhile(_.height < height).length + 1))

  override lazy val pt1: Int =
    trees
      .map(_.isVisible(trees))
      .count(_ == true)

  override lazy val pt2: Int =
    trees
      .map(_.scenicScore(trees))
      .iterate
      .max
