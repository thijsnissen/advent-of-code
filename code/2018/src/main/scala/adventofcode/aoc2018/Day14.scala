package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day14 extends AdventOfCode(Prod):
  val recipesCount: Int =
    input
      .linesIterator
      .map(_.toInt)
      .next

  object Recipes:
    extension (self: Int)
      def fromInt: Vector[Int] =
        self
          .toString
          .map(_.asDigit)
          .toVector

  import Recipes.fromInt

  @annotation.tailrec
  def loop(
    scoreboard: Vector[Int],
    elves: Vector[Int]
  )(f: Vector[Int] => Boolean): Vector[Int] =
    if f(scoreboard) then
      scoreboard
    else
      val newRecipes =
        elves.map(scoreboard)

      val newScoreboard =
        scoreboard ++ newRecipes.sum.fromInt

      val newElves =
        elves.zip(newRecipes).map: (i, s) =>
          (i + 1 + s) % newScoreboard.length

      loop(newScoreboard, newElves)(f)

  override lazy val pt1: String =
    val result =
      loop(Vector(3, 7), Vector(0, 1)): s =>
        s.length >= recipesCount + 10

    result
      .slice(recipesCount, recipesCount + 10)
      .mkString

  override lazy val pt2: Int =
    val recipesCountSlice =
      recipesCount.fromInt

    val elves  = Vector(0, 1)
    val offset = elves.length - 1

    val result =
      loop(Vector(3, 7), elves): s =>
        s.takeRight(recipesCountSlice.length + offset).containsSlice(
          recipesCountSlice
        )

    result.indexOfSlice(recipesCountSlice)
