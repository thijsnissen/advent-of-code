package adventofcode
package aoc2022

import utilities.AdventOfCode.*

object Day02 extends AdventOfCode(Prod):
  val strategy: Vector[(Game, Game)] =
    input
      .linesIterator
      .collect:
        case s"$a $b" =>
          (Game.fromInput(a.charAt(0)), Game.fromInput(b.charAt(0)))
      .toVector

  enum Game:
    case Rock
    case Paper
    case Scissors

  object Game:
    def fromInput(c: Char): Game =
      c match
        case 'A' | 'X' => Game.Rock
        case 'B' | 'Y' => Game.Paper
        case 'C' | 'Z' => Game.Scissors

    def playPart1(op: Game, me: Game): Int =
      if Game.fromOrdinal((op.ordinal + 1) % 3) == me then
        1 + me.ordinal + 6
      else if op == me then
        1 + me.ordinal + 3
      else
        1 + me.ordinal

    def playPart2(op: Game, me: Game): Int =
      if me == Game.Scissors then // Z == win
        1 + Game.fromOrdinal((op.ordinal + 1) % 3).ordinal + 6
      else if me == Game.Paper then // Y == draw
        1 + op.ordinal + 3
      else // X == lose
        1 + Game.fromOrdinal((op.ordinal + 2) % 3).ordinal

  override lazy val pt1: Int =
    strategy
      .map(Game.playPart1)
      .sum

  override lazy val pt2: Int =
    strategy
      .map(Game.playPart2)
      .sum
