package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Grid
import utilities.Grid.*
import utilities.Pos

object Day20 extends AdventOfCode(Prod):
  val raceTrack: Grid[Char] =
    Grid.unit:
      input.linesIterator.map(_.toVector).toVector

  extension (self: Grid[Char])
    def next(p: Pos): Set[Pos] =
      p.axisOffsetsFn: p =>
        self.lift(p.x, p.y).exists(Set('.', 'S', 'E').contains)

    def race(s: Pos, e: Pos): Map[Pos, Int] =
      @tailrec def loop(
        todo: Vector[(Pos, Int)],
        path: Map[Pos, Int]
      ): Map[Pos, Int] =
        todo.head match
          case (p, d) if p == e            => path + (p -> d)
          case (p, d) if !path.contains(p) =>
            loop(todo.tail ++ next(p).map(_ -> (d + 1)), path + (p -> d))
          case _ => loop(todo.tail, path)

      loop(Vector((s, 0)), Map.empty[Pos, Int])

  extension (self: Map[Pos, Int])
    def cheats(ps: Int): Vector[Int] =
      val keys = self.keys.toVector

      @tailrec def loop(todo: Vector[Pos], saved: Vector[Int]): Vector[Int] =
        todo.headOption match
          case None    => saved
          case Some(p) =>
            val c =
              keys
                .filter(q => p.manhattan(q) <= ps)
                .map(q => self(q) - self(p) - p.manhattan(q).toInt)

            loop(todo.tail, saved ++ c)

      loop(keys, Vector.empty[Int])

  lazy val pt1: Int =
    raceTrack
      .race(
        s = raceTrack.findPos('S'),
        e = raceTrack.findPos('E')
      )
      .cheats(ps = 2)
      .count(_ >= (if getEnv == Test then 20 else 100))

  lazy val pt2: Int =
    raceTrack
      .race(
        s = raceTrack.findPos('S'),
        e = raceTrack.findPos('E')
      )
      .cheats(ps = 20)
      .count(_ >= (if getEnv == Test then 50 else 100))

  answer(1)(pt1)

  answer(2)(pt2)
