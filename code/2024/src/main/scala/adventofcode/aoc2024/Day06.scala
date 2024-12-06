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
    map: Grid[Char],
    startPos: Pos,
    startDir: Char,
    maxSteps: Int = 10000
  ): Set[Pos] =
    @tailrec def loop(
      p: Pos,
      d: Char,
      acc: Set[Pos],
      steps: Int = 0
    ): Set[Pos] =
      if steps >= maxSteps then Set.empty[Pos]
      else
        val np = p + nextPos(d)

        map.lift(np.x, np.y) match
          case Some('#') => loop(p, nextDir(d), acc, steps)
          case Some('.') => loop(np, d, acc + p, steps + 1)
          case _         => acc + p

    loop(startPos, startDir, Set.empty[Pos])

  def findGuard(map: Grid[Char]): (Pos, Char) =
    map
      .findWithIndex((_, _, c) => Set('^', '>', 'v', '<').contains(c))
      .map((x, y, d) => Pos(x, y) -> d)
      .get

  lazy val pt1: Int =
    val (guardPos, guardDir) = findGuard(labMap)
    val labMapWithoutGuard   = labMap.set(guardPos.x, guardPos.y)('.')

    walk(labMapWithoutGuard, guardPos, guardDir).size

  lazy val pt2: Int =
    val (guardPos, guardDir) = findGuard(labMap)

    val labMapWithoutGuard =
      labMap.set(guardPos.x, guardPos.y)('.')

    val result: Iterator[Int] =
      labMapWithoutGuard.iterateWithIndex.map: (x, y, c) =>
        if c != '.' then 1
        else
          walk(
            labMapWithoutGuard.set(x, y)('#'),
            guardPos,
            guardDir
          ).size

    result.count(_ == 0)

  answer(1)(pt1)

  answer(2)(pt2)
