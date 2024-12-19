package adventofcode
package aoc2024

import scala.collection.mutable
import utilities.AdventOfCode.*
import utilities.Grid
import utilities.Grid.*
import utilities.Pos
import utilities.Utilities.+%

object Day16 extends AdventOfCode(Test):
  val maze: Grid[Char] =
    Grid.unit:
      input
        .linesIterator
        .map(_.toVector)
        .toVector

  enum Direction:
    case North, East, South, West

    def offset: Pos =
      this match
        case Direction.North => Pos(0, -1)
        case Direction.East  => Pos(1, 0)
        case Direction.South => Pos(0, 1)
        case Direction.West  => Pos(-1, 0)

  case class Path(
    dir: Direction,
    pos: Pos,
    score: Int,
    trace: Map[Pos, Pos] = Map.empty[Pos, Pos]
  ):
    lazy val key: (Pos, Direction) =
      (pos, dir)

    def path(pos: Pos): Vector[Pos] =
      @tailrec def loop(p: Pos, acc: Vector[Pos]): Vector[Pos] =
        trace.get(p) match
          case Some(p) => loop(p, acc :+ p)
          case None    => acc

      loop(pos, Vector(pos))

    def offset(maze: Grid[Char]): Vector[Path] =
      Vector(
        (dir, 1),
        (Direction.fromOrdinal((dir.ordinal + 1) +% 4), 1001),
        (Direction.fromOrdinal((dir.ordinal + 2) +% 4), 2001),
        (Direction.fromOrdinal((dir.ordinal + 3) +% 4), 1001)
      )
        .map: (d, s) =>
          Path(d, pos + d.offset, score + s, trace.updated(pos, pos + d.offset))
        .filter: p =>
          maze.lift(p.pos.x, p.pos.y).exists(Set('S', 'E', '.').contains)

  extension (self: Grid[Char])
    def find(start: Pos, end: Pos, dir: Direction): Vector[Path] =
      val queue =
        mutable.PriorityQueue(Path(dir, start, 0)):
          Ordering.by(p => -p.score)

      @tailrec def loop(
        acc: Vector[Path],
        scores: Map[(Pos, Direction), Int]
      ): Vector[Path] =
        if queue.isEmpty then acc
        else
          queue.dequeue() match
            case path if scores.get(path.key).exists(path.score > _) =>
              loop(acc, scores)
            case path if path.pos == end =>
              loop(acc :+ path, scores.updated(path.key, path.score))
            case path =>
              queue.enqueue(path.offset(self)*)

              loop(acc, scores.updated(path.key, path.score))

      loop(Vector.empty[Path], Map.empty[(Pos, Direction), Int])

    def lowestScore(dir: Direction): Int =
      find(self.findPos('S'), self.findPos('E'), dir)
        .head
        .score

    def bestSeats(dir: Direction): Int =
      val start    = self.findPos('S')
      val paths    = find(start, self.findPos('E'), dir)
      val minScore = paths.minBy(_.score).score

      paths
        .takeWhile(_.score == minScore)
        .flatMap(_.path(start))
        .distinct
        .length

  lazy val pt1: Int =
    maze.lowestScore(dir = Direction.East)

  lazy val pt2: Int =
    maze.bestSeats(dir = Direction.East)

  answer(1)(pt1)

  answer(2)(pt2)
