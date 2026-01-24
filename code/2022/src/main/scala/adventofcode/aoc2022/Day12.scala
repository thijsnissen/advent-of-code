package adventofcode
package aoc2022

import utilities.AdventOfCode.*
import utilities.Graph
import utilities.GraphTraversal.*
import utilities.Orderings.posReadingOrder
import utilities.Pos

object Day12 extends AdventOfCode(Prod):
  val grid: Vector[Square] =
    val result: Iterator[Square] =
      for
        (r, y) <- input.linesIterator.zipWithIndex
        (c, x) <- r.zipWithIndex
      yield Square(Pos(x, y), c)

    result.toVector

  case class Square(pos: Pos, elevation: Char)

  object Square:
    def normalize: Char => Char =
      case 'S'   => 'a'
      case 'E'   => 'z'
      case other => other

    def canMove(from: Square, to: Square): Boolean =
      normalize(to.elevation) >= normalize(from.elevation) - 1

    given Ordering[Square] = Ordering.by(_.pos)

  def graph: Graph[Square] =
    Graph.unit: (square: Square) =>
      val offset: Set[Pos] = square.pos.axisOffsets

      scala.collection.immutable.SortedSet.empty[Square] ++
        grid.filter(s => offset.contains(s.pos) && Square.canMove(square, s))

  def fewestStepsToBestSignal(start: Square)(target: Square => Boolean): Int =
    graph.breadthFirstSearchPathTo(start)(target) match
      case Some(path) => path.length - 1
      case None       => Int.MaxValue

  // For both parts I find the path backwards,
  // from target position to start position.

  override lazy val pt1: Int =
    val start: Square  = grid.find(_.elevation == 'S').get
    val target: Square = grid.find(_.elevation == 'E').get

    fewestStepsToBestSignal(target)(_ == start)

  override lazy val pt2: Int =
    val start: Square => Boolean =
      s => Square.normalize(s.elevation) == 'a'

    val target: Square =
      grid.find(_.elevation == 'E').get

    fewestStepsToBestSignal(target)(start)
