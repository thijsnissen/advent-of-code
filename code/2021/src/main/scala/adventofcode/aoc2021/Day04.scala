package adventofcode
package aoc2021

import utilities.AdventOfCode.*

object Day04 extends AdventOfCode(Prod):
  val numbers: Vector[Int] =
    input
      .linesIterator
      .next
      .split(",")
      .map(_.toInt)
      .toVector

  val boards: Vector[Board] =
    input
      .split("\n\n")
      .drop(1)
      .map: (s: String) =>
        val b: Board =
          Board.fromString(s)

        b ++ b.transpose
      .toVector

  type Board = Vector[Vector[Int]]

  object Board:
    def fromString(s: String): Board =
      s
        .linesIterator
        .map: (l: String) =>
          l
            .split(" ")
            .filter(_.nonEmpty)
            .map(_.toInt)
            .toVector
        .toVector

    @tailrec def bingo(todo: Vector[Int], boards: Vector[Board]): Int =
      val i: Int              = todo.head
      val game: Vector[Board] = boards.map(_.play(i))

      game.find(_.isWinner) match
        case Some(board: Board) => board.score * i
        case None               => bingo(todo.tail, game)

    @tailrec def lastBoard(
      todo: Vector[Int],
      boards: Vector[Board]
    ): (Vector[Int], Vector[Board]) =
      val i: Int              = todo.head
      val game: Vector[Board] = boards.map(_.play(i)).filterNot(_.isWinner)

      if game.sizeIs == 1 then (todo.tail, game) else lastBoard(todo.tail, game)

    extension (self: Board)
      def play(i: Int): Board =
        self.map(_.filterNot(_ == i))

      def isWinner: Boolean =
        self.exists(_.isEmpty)

      def score: Int =
        self.flatten.distinct.sum

  import Board.*

  lazy val pt1: Int =
    bingo(numbers, boards)

  lazy val pt2: Int =
    bingo.tupled(lastBoard(numbers, boards))

  answer(1)(pt1)

  answer(2)(pt2)
