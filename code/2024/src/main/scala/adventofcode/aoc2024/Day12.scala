package adventofcode
package aoc2024

import utilities.AdventOfCode.*
import utilities.Grid
import utilities.Grid.*
import utilities.Pos
import utilities.Utilities.sumBy

object Day12 extends AdventOfCode(Prod):
  val gardenPlots: Grid[Char] =
    Grid.unit:
      input
        .linesIterator
        .map(_.toVector)
        .toVector

  type Plot   = (pos: Pos, perimeter: Int)
  type Region = Vector[Plot]

  extension (self: Grid[Char])
    def offsets(p: Pos): Set[Pos] =
      p.axisOffsetsFn(n => self.lift(n.x, n.y).contains(self(p.x)(p.y)))

    def region(p: Pos): Region =
      @tailrec def loop(todo: Vector[Pos], acc: Region): Region =
        todo.headOption match
          case None => acc
          case Some(pos) if !acc.exists(_.pos == pos) =>
            val n = offsets(pos)

            loop(todo.tail ++ n, acc :+ (pos, 4 - n.size))
          case Some(_) => loop(todo.tail, acc)

      loop(Vector(p), Vector.empty[Plot])

    def regions: Vector[Region] =
      self.iterateWithIndex.foldLeft(Vector.empty[Region]):
        case (acc, (x, y, _)) =>
          if acc.exists(_.exists(_.pos == Pos(x, y))) then acc
          else acc :+ region(Pos(x, y))

  object Region:
    extension (self: Region)
      def has(p: Pos): Boolean =
        self.exists(plot => plot.pos == p)

      // This idea is credited to @jan-pieter
      def sides: Int =
        self
          .map: plot =>
            val topLeftOutside =
              !self.has(plot.pos + Pos(0, -1)) &&
                !self.has(plot.pos + Pos(-1, 0))

            val bottomRightInside =
              self.has(plot.pos + Pos(0, -1)) &&
                self.has(plot.pos + Pos(-1, 0)) &&
                !self.has(plot.pos + Pos(-1, -1))

            val topRightOutside =
              !self.has(plot.pos + Pos(0, -1)) &&
                !self.has(plot.pos + Pos(1, 0))

            val bottomLeftInside =
              self.has(plot.pos + Pos(0, -1)) &&
                self.has(plot.pos + Pos(1, 0)) &&
                !self.has(plot.pos + Pos(1, -1))

            val bottomLeftOutside =
              !self.has(plot.pos + Pos(0, 1)) &&
                !self.has(plot.pos + Pos(-1, 0))

            val topRightInside =
              self.has(plot.pos + Pos(0, 1)) &&
                self.has(plot.pos + Pos(-1, 0)) &&
                !self.has(plot.pos + Pos(-1, 1))

            val bottomRightOutside =
              !self.has(plot.pos + Pos(0, 1)) &&
                !self.has(plot.pos + Pos(1, 0))

            val topLeftInside =
              self.has(plot.pos + Pos(0, 1)) &&
                self.has(plot.pos + Pos(1, 0)) &&
                !self.has(plot.pos + Pos(1, 1))

            topLeftOutside :: bottomRightInside :: topRightOutside ::
              bottomLeftInside :: bottomLeftOutside :: topRightInside ::
              bottomRightOutside :: topLeftInside :: Nil count identity
          .sum

      def price(withDiscount: Boolean): Int =
        if withDiscount then self.length * self.sides
        else self.length * self.sumBy(_.perimeter)

  import Region.*

  lazy val pt1: Int =
    gardenPlots
      .regions
      .sumBy(_.price(withDiscount = false))

  lazy val pt2: Int =
    gardenPlots
      .regions
      .sumBy(_.price(withDiscount = true))

  answer(1)(pt1)

  answer(2)(pt2)
