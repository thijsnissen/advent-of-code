package adventofcode
package aoc2024

import scala.util.Random
import utilities.AdventOfCode.*

object Day23 extends AdventOfCode(Prod):
  val computers: LANParty =
    LANParty.fromIterator(input.linesIterator)

  type LANParty = Map[String, Set[String]]

  object LANParty:
    lazy val Empty: LANParty =
      Map.empty[String, Set[String]]
        .withDefaultValue(Set.empty[String])

    def fromIterator(cs: Iterator[String]): LANParty =
      cs.foldLeft(LANParty.Empty):
        case (acc, s"$a-$b") =>
          acc.updated(a, acc(a) + b).updated(b, acc(b) + a)
        case (_, _) => sys.error("BOOM!")

    extension (self: Set[String])
      def password: String =
        self.toVector.sorted.mkString(",")

    extension (self: LANParty)
      def cliquesOfThree: Set[Set[String]] =
        self.foldLeft(Set.empty[Set[String]]):
          case (acc, (c, cs)) =>
            (for
              n1 <- cs
              n2 <- computers(n1)
              if computers(n2).contains(c)
            yield Set(c, n1, n2)) ++ acc

      def maxCliques: Set[Set[String]] =
        // r: current clique
        // p: candidate set
        // x: exclusion set
        // u: pivot
        // see: https://www.youtube.com/watch?v=j_uQChgo72I
        def bronKerbosch(
          r: Set[String],
          p: Set[String],
          x: Set[String],
          acc: Set[Set[String]]
        ): Set[Set[String]] =
          if p.isEmpty && x.isEmpty then acc + r
          else
            val (_, _, res) =
              val u = Random.shuffle(p ++ x).head

              (p -- self(u)).foldLeft((p, x, acc)):
                case ((p, x, acc), v) =>
                  (
                    p - v,
                    x + v,
                    bronKerbosch(
                      r + v,
                      p.intersect(self(v)),
                      x.intersect(self(v)),
                      acc
                    )
                  )

            res

        bronKerbosch(Set.empty, self.keySet, Set.empty, Set.empty)

  import LANParty.*

  lazy val pt1: Int =
    computers
      .cliquesOfThree
      .count(_.exists(_.startsWith("t")))

  lazy val pt2: String =
    computers
      .maxCliques
      .maxBy(_.size)
      .password

  answer(1)(pt1)

  answer(2)(pt2)
