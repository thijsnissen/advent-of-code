package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Pos

object Day04 extends AdventOfCode(Prod):
  val wordSearch: WordSearch =
    (for
      (l, y) <- input.linesIterator.zipWithIndex
      (c, x) <- l.zipWithIndex
    yield Pos(x, y) -> c).toMap

  enum Direction:
    case Hor, Ver, DgL, DgR

  object Direction:
    extension (self: Direction)
      def offset(pos: Pos)(f: Pos => Boolean): Set[Pos] =
        val offset =
          self match
            case Hor => Set(pos.copy(x = pos.x - 1), pos.copy(x = pos.x + 1))
            case Ver => Set(pos.copy(y = pos.y - 1), pos.copy(y = pos.y + 1))
            case DgL =>
              Set(Pos(pos.x - 1, pos.y + 1), Pos(pos.x + 1, pos.y - 1))
            case DgR =>
              Set(Pos(pos.x - 1, pos.y - 1), Pos(pos.x + 1, pos.y + 1))

        offset.filter(f)

  type WordSearch = Map[Pos, Char]

  object WordSearch:
    extension (self: WordSearch)
      def allPos(char: Char): Set[Pos] =
        self
          .filter((_, c) => c == char)
          .keys
          .toSet

      def findXmas: Int =
        @tailrec def loop(xs: Vector[(Pos, Direction)], acc: Int = 0): Int =
          xs.headOption match
            case None         => acc
            case Some(x, dir) =>
              val s =
                for
                  m <- dir.offset(x)(self.get(_).contains('M'))
                  a <- dir.offset(m)(self.get(_).contains('A'))
                yield dir.offset(a)(self.get(_).contains('S'))

              loop(xs.tail, acc + s.toVector.flatten.length)

        val xs =
          self.allPos('X').toVector

        loop(
          xs.map(_ -> Direction.Hor) ++ xs.map(_ -> Direction.Ver) ++
            xs.map(_ -> Direction.DgL) ++ xs.map(_ -> Direction.DgR)
        )

      def findXedMas: Int =
        def isMas(p: Pos)(d: Direction): Boolean = (d.offset(p)(_ => true) + p)
          .flatMap(self.get)
          .intersect(Set('M', 'A', 'S'))
          .size == 3

        @tailrec def loop(ps: Vector[Pos], acc: Int = 0): Int =
          ps.headOption match
            case None    => acc
            case Some(p) =>
              loop(
                ps.tail,
                if isMas(p)(Direction.DgR) && isMas(p)(Direction.DgL)
                then acc + 1
                else acc
              )

        loop(self.allPos('A').toVector)

  import WordSearch.*

  override lazy val pt1: Int =
    wordSearch.findXmas

  override lazy val pt2: Int =
    wordSearch.findXedMas
