package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Grid
import utilities.Grid.*
import utilities.Pos

object Day15 extends AdventOfCode(Prod):
  val (warehouse: Grid[Char], moves: Vector[Pos]) =
    val Array(w, m) =
      input.split("\n\n")

    (
      Grid.unit(w.linesIterator.map(_.toVector).toVector),
      m.filterNot(_ == '\n').toVector.map:
        case '^' => Pos(0, -1)
        case '>' => Pos(1, 0)
        case 'v' => Pos(0, 1)
        case '<' => Pos(-1, 0)
    )

  extension (self: Grid[Char])
    def move(c: Pos, m: Pos): (s: Grid[Char], r: Pos) =
      val n = c + m

      (self.lift(n.x, n.y), m) match
        case (Some('.'), _) => (self.swap(c.x, c.y)(n.x, n.y), n)
        case (Some('O'), _) | (Some('[') | Some(']'), Pos(_, 0)) =>
          val ns = move(n, m).s

          if ns.lift(n.x, n.y).contains('.') then
            (ns.swap(c.x, c.y)(n.x, n.y), n)
          else (self, c)
        case (Some(b), Pos(0, _)) if Set('[', ']').contains(b) =>
          val xo = if b == '[' then 1 else -1
          val ns = move(n.copy(x = n.x + xo), m).s.move(n, m).s

          if
            ns.lift(n.x, n.y).contains('.') &&
            ns.lift(n.x + xo, n.y).contains('.')
          then
            (
              ns
                .swap(c.x, c.y)(n.x, n.y)
                .swap(c.x, c.y)(n.x + xo, n.y),
              n
            )
          else (self, c)
        case _ => (self, c)

    def move(ms: Vector[Pos]): Grid[Char] =
      ms
        .foldLeft((s = self, r = self.findPos('@'))): (acc, m) =>
          acc.s.move(acc.r, m)
        .s

    def GpsCoordinates: Vector[Int] =
      self.iterateWithIndex.foldLeft(Vector.empty[Int]):
        case (acc, (x, y, c)) =>
          c match
            case 'O' | '[' => acc :+ y * 100 + x
            case _         => acc

    def expand: Grid[Char] =
      self.flatMap:
        case '#' => Vector('#', '#')
        case 'O' => Vector('[', ']')
        case '.' => Vector('.', '.')
        case '@' => Vector('@', '.')

  lazy val pt1: Int =
    warehouse
      .move(moves)
      .GpsCoordinates
      .sum

  lazy val pt2: Int =
    warehouse
      .expand
      .move(moves)
      .GpsCoordinates
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
