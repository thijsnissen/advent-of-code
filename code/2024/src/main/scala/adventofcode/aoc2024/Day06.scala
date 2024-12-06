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

  def nextPos(c: Char): Pos =
    c match
      case '^' => Pos(0, -1)
      case '>' => Pos(1, 0)
      case 'v' => Pos(0, 1)
      case '<' => Pos(-1, 0)

  def nextDir(c: Char): Char =
    c match
      case '^' => '>'
      case '>' => 'v'
      case 'v' => '<'
      case '<' => '^'

  def walk(
    labMap: Grid[Char],
    startPos: Pos,
    startDir: Char
  ): Option[Set[Pos]] =
    @tailrec def loop(
      p: Pos,
      d: Char,
      acc: Set[(Pos, Char)]
    ): Option[Set[Pos]] =
      if acc.contains(p -> d) then None
      else
        val np = p + nextPos(d)

        labMap.lift(np.x, np.y) match
          case Some('#') =>
            loop(p, nextDir(d), acc)
          case Some(c) if c == '.' || c == startDir =>
            loop(np, d, acc + (p -> d))
          case _ => Some(acc.map((p, _) => p) + p)

    loop(startPos, startDir, Set.empty[(Pos, Char)])

  def findGuard(map: Grid[Char]): (Pos, Char) =
    map
      .findWithIndex((_, _, c) => Set('^', '>', 'v', '<').contains(c))
      .map((x, y, d) => Pos(x, y) -> d)
      .get

  lazy val pt1: Int =
    val (guardPos, guardDir) =
      findGuard(labMap)

    walk(labMap, guardPos, guardDir)
      .get
      .size

  lazy val pt2: Int =
    val (guardPos, guardDir) = findGuard(labMap)

    walk(labMap, guardPos, guardDir)
      .get
      .toVector
      .map:
        case Pos(x, y) =>
          walk(labMap.set(x, y)('#'), guardPos, guardDir)
      .count(_.isEmpty)

  answer(1)(pt1)

  answer(2)(pt2)
