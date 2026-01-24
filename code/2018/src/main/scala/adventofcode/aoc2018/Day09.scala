package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day09 extends AdventOfCode(Prod):
  val myInput: List[Game] =
    input
      .linesIterator
      .map(Game.fromString)
      .toList

  case class Game(
    players: Map[Int, Long],
    player: Int,
    game: Vector[Long],
    lastMarble: Long
  ):
    def play(multiplier: Int = 1): Game =
      (1L to lastMarble * multiplier).foldLeft(this):
        case (g, m) if m % 23 == 0 =>
          val (score, board) = g.removeMarble(-7)

          g.copy(
            players = g.players.updatedWith(g.player)(_.map(_ + m + score)),
            player = g.nextPlayer,
            game = board
          )
        case (g, m) =>
          g.copy(player = g.nextPlayer, game = g.addMarble(m))

    def addMarble(value: Long): Vector[Long] =
      val (l, r) = game.splitAt(2)

      (value +: r) ++ l

    def removeMarble(index: Long): (Long, Vector[Long]) =
      val (l, r) = game.splitAt(
        (((index % game.length) + game.length) % game.length).toInt
      )

      (r.head, r.tail ++ l)

    def nextPlayer: Int =
      player % players.size + 1

  private object Game:
    def fromString(s: String): Game =
      s match
        case s"$players players; last marble is worth $lastMarble points" =>
          Game(
            (1 to players.toInt).map((_, 0L)).toMap,
            player = 1,
            Vector[Long](0),
            lastMarble.toLong
          )

  override lazy val pt1: Long =
    myInput
      .head
      .play()
      .players
      .maxBy((_, score) => score)
      ._2

  override lazy val pt2: Long =
    myInput
      .head
      .play(100)
      .players
      .maxBy((_, score) => score)
      ._2
