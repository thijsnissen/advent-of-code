package adventofcode
package aoc2024

import utilities.AdventOfCode.*

object Day25 extends AdventOfCode(Prod):
  val tumblers: Vector[Tumbler] =
    input
      .split("\n\n")
      .map(Tumbler.fromString)
      .toVector

  enum Kind:
    case Key, Lock

  object Kind:
    def fromString(s: String): Kind =
      if s.startsWith(".") then Key else Lock

  case class Tumbler(a: Int, b: Int, c: Int, d: Int, e: Int, kind: Kind):
    def fits(that: Tumbler): Boolean =
      kind != that.kind && List(
        a + that.a,
        b + that.b,
        c + that.c,
        d + that.d,
        e + that.e
      ).forall(_ <= 7)

  object Tumbler:
    def fromString(s: String): Tumbler =
      val Vector(a, b, c, d, e) =
        s
          .linesIterator
          .toVector
          .transpose
          .map(_.count(_ == '#'))

      Tumbler(a, b, c, d, e, Kind.fromString(s))

    extension (self: Vector[Tumbler])
      def fits: Int =
        self.combinations(2).count:
          case Seq(a, b) => a.fits(b)
          case _         => sys.error("BOOM!")

  import Tumbler.*

  override lazy val pt1: Long =
    tumblers.fits
