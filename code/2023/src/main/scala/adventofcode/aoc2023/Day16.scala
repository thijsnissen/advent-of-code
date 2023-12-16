package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Grid

object Day16 extends AdventOfCode(Prod):
  import Beam.*
  import Direction.*

  val layout: Grid[Char] =
    Grid.unit:
      input
        .linesIterator
        .map(_.toVector)
        .toVector

  enum Direction:
    case Up, Right, Down, Left

  case class Beam(x: Int, y: Int, d: Direction)

  object Beam:
    extension (self: Grid[Char])
      @tailrec def move(
        todo: List[Beam],
        acc: Set[Beam] = Set.empty[Beam]
      ): Set[Beam] =
        if todo.isEmpty then acc
        else if acc.contains(todo.head) then move(todo.tail, acc)
        else
          val Beam(x: Int, y: Int, d: Direction) = todo.head

          val next: List[Beam] =
            (self.lift(x, y), d) match
              case (Some('/'), Up) | (Some('\\'), Down) =>
                Beam(x + 1, y, Right) :: Nil
              case (Some('/'), Right) | (Some('\\'), Left) =>
                Beam(x, y - 1, Up) :: Nil
              case (Some('/'), Left) | (Some('\\'), Right) =>
                Beam(x, y + 1, Down) :: Nil
              case (Some('/'), Down) | (Some('\\'), Up) =>
                Beam(x - 1, y, Left) :: Nil

              case (Some('|'), Left) | (Some('|'), Right) =>
                Beam(x, y - 1, Up) :: Beam(x, y + 1, Down) :: Nil
              case (Some('-'), Up) | (Some('-'), Down) =>
                Beam(x - 1, y, Left) :: Beam(x + 1, y, Right) :: Nil

              case (Some('.'), Up) | (Some('|'), Up) =>
                Beam(x, y - 1, Up) :: Nil
              case (Some('.'), Down) | (Some('|'), Down) =>
                Beam(x, y + 1, Down) :: Nil
              case (Some('.'), Right) | (Some('-'), Right) =>
                Beam(x + 1, y, Right) :: Nil
              case (Some('.'), Left) | (Some('-'), Left) =>
                Beam(x - 1, y, Left) :: Nil

              case _ => List.empty[Beam]

          move(
            next ::: todo.tail,
            if next.isEmpty then acc else acc + todo.head
          )

      def energized(start: Beam): Int =
        self
          .move(start :: Nil)
          .map((b: Beam) => (b.x, b.y))
          .size

  lazy val pt1: Int =
    layout.energized(Beam(0, 0, Right))

  lazy val pt2: Int =
    val (xMax: Int, yMax: Int) = layout.size

    val startingBeams: List[Beam] = List(
      (0 until xMax).map((x: Int) => Beam(x, 0, Down)),
      (0 until xMax).map((x: Int) => Beam(x, yMax, Up)),
      (0 until yMax).map((y: Int) => Beam(0, y, Right)),
      (0 until yMax).map((y: Int) => Beam(xMax, y, Left))
    ).flatten

    startingBeams.map(layout.energized).max

  answer(1)(pt1)

  answer(2)(pt2)
