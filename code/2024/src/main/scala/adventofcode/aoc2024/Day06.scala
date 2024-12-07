package adventofcode
package aoc2024

import adventofcode.utilities.AdventOfCode.*
import adventofcode.utilities.Grid
import adventofcode.utilities.Grid.*
import adventofcode.utilities.Pos

object Day06 extends AdventOfCode(Prod):
  val labMap: Grid[Char] =
    Grid.unit:
      input
        .linesIterator
        .map(_.toVector)
        .toVector

  val dirs: Map[Pos, Pos] =
    Map(
      Pos(0, -1) -> Pos(1, 0),
      Pos(1, 0)  -> Pos(0, 1),
      Pos(0, 1)  -> Pos(-1, 0),
      Pos(-1, 0) -> Pos(0, -1)
    )

  def walk(labMap: Grid[Char], startPos: Pos, startDir: Pos): Option[Set[Pos]] =
    @tailrec def loop(p: Pos, d: Pos, acc: Set[(Pos, Pos)]): Option[Set[Pos]] =
      if acc.contains(p -> d) then None
      else
        val np = p + d

        labMap.lift(np.x, np.y) match
          case Some('#') =>
            loop(p, dirs(d), acc)
          case Some(c) if c == '.' || c == '^' =>
            loop(np, d, acc + (p -> d))
          case _ =>
            Some(acc.map((p, _) => p) + p)

    loop(startPos, startDir, Set.empty[(Pos, Pos)])

  def findGuard(labMap: Grid[Char]): Pos =
    labMap
      .findWithIndex((_, _, c) => c == '^')
      .map((x, y, _) => Pos(x, y))
      .get

  lazy val pt1: Int =
    walk(labMap, findGuard(labMap), Pos(0, -1))
      .get
      .size

  lazy val pt2: Int =
    val guardPos   = findGuard(labMap)
    val initialDir = Pos(0, -1)

    walk(labMap, guardPos, initialDir)
      .get
      .toSeq
      .map:
        case Pos(x, y) =>
          walk(labMap.set(x, y)('#'), guardPos, initialDir)
      .count(_.isEmpty)

  answer(1)(pt1)

  answer(2)(pt2)
