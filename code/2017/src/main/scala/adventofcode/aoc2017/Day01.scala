package adventofcode
package aoc2017

import utilities.AdventOfCode.*

object Day01 extends AdventOfCode(Prod):
  val captcha: Vector[Int] =
    input
      .trim
      .map(_.asDigit)
      .toVector

  extension (self: Vector[Int])
    def zipWithOffset(n: Int): Vector[(Int, Int)] =
      self
        .zipWithIndex
        .map((x, i) => (x, self((i + n) % self.length)))

  extension (self: Vector[(Int, Int)])
    def solve: Int =
      self.foldLeft(0):
        case (acc, (a, b)) if a == b => acc + a
        case (acc, _)                => acc

  override lazy val pt1: Int =
    captcha
      .zipWithOffset(1)
      .solve

  override lazy val pt2: Int =
    captcha
      .zipWithOffset(captcha.length / 2)
      .solve
