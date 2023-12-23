package adventofcode
package aoc2023

import utilities.AdventOfCode.*
import utilities.Range
import utilities.Range.*

object Day19 extends AdventOfCode(Prod):
  val workflows: Map[String, Workflow] =
    input
      .split("\n\n")
      .head
      .linesIterator
      .map(Workflow.fromString)
      .toMap

  val parts: Vector[Part] =
    input
      .split("\n\n")
      .drop(1)
      .head
      .linesIterator
      .map(Part.fromString)
      .toVector

  case class Workflow(rules: Vector[Rule], default: String):
    @tailrec final def evaluate(part: Part): String =
      rules.headOption match
        case Some(rule) if rule.evaluate(part) => rule.target
        case Some(_) => copy(rules = rules.tail).evaluate(part)
        case None    => default

    def evaluateRanges(ranges: Ranges): Vector[(String, Ranges)] =
      @tailrec def loop(
        rules: Vector[Rule],
        ranges: Ranges,
        next: Vector[(String, Ranges)]
      ): Vector[(String, Ranges)] =
        rules.headOption match
          case None => next :+ (default, ranges)
          case Some(rule) =>
            val (rTrue, rFalse) = rule.split(ranges)

            loop(rules.tail, rFalse, next :+ (rule.target, rTrue))

      loop(rules, ranges, Vector.empty[(String, Ranges)])

  object Workflow:
    def fromString(s: String): (String, Workflow) =
      s match
        case s"$name{$rules}" =>
          val r: Vector[String] = rules.split(",").toVector

          name -> Workflow(r.init.map(Rule.fromString), r.last)

  case class Rule(category: Char, op: Char, condition: Int, target: String):
    def evaluate(part: Part): Boolean =
      op match
        case '>' => part.category(category) > condition
        case '<' => part.category(category) < condition

    def split(ranges: Ranges): (Ranges, Ranges) =
      val Range(min, max) = ranges.category(category)

      op match
        case '>' => (
            ranges.updated(category, condition + 1 to max),
            ranges.updated(category, min to condition)
          )
        case '<' => (
            ranges.updated(category, min to condition - 1),
            ranges.updated(category, condition to max)
          )

  object Rule:
    def fromString(s: String): Rule =
      s match
        case s"$category>$condition:$target" =>
          Rule(category.charAt(0), '>', condition.toInt, target)
        case s"$category<$condition:$target" =>
          Rule(category.charAt(0), '<', condition.toInt, target)

  trait withCategory[A]:
    val x: A; val m: A; val a: A; val s: A

    def category(c: Char): A =
      c match
        case 'x' => x
        case 'm' => m
        case 'a' => a
        case 's' => s

  case class Part(x: Int, m: Int, a: Int, s: Int) extends withCategory[Int]:
    lazy val totalRating: Int = x + m + a + s

  object Part:
    def fromString(s: String): Part =
      s match
        case s"{x=$x,m=$m,a=$a,s=$s}" =>
          Part(x.toInt, m.toInt, a.toInt, s.toInt)

    extension (self: Vector[Part])
      def acceptedParts: Vector[Part] =
        @tailrec def loop(
          todo: Vector[(String, Part)],
          accepted: Vector[Part]
        ): Vector[Part] =
          todo.headOption match
            case None            => accepted
            case Some("A", part) => loop(todo.tail, accepted :+ part)
            case Some("R", _)    => loop(todo.tail, accepted)
            case Some(workflow, part) => loop(
                todo.tail :+ (workflows(workflow).evaluate(part), part),
                accepted
              )

        loop(self.map("in" -> _), Vector.empty[Part])

  case class Ranges(x: Range, m: Range, a: Range, s: Range)
      extends withCategory[Range]:
    lazy val combinations: Long = x.size * m.size * a.size * s.size

    def updated(c: Char, r: Range): Ranges =
      c match
        case 'x' => copy(x = r)
        case 'm' => copy(m = r)
        case 'a' => copy(a = r)
        case 's' => copy(s = r)

  object Ranges:
    extension (self: Map[String, Workflow])
      def acceptedRanges: Vector[Ranges] =
        @tailrec def loop(
          todo: Vector[(String, Ranges)],
          accepted: Vector[Ranges]
        ): Vector[Ranges] =
          todo.headOption match
            case None              => accepted
            case Some("R", _)      => loop(todo.tail, accepted)
            case Some("A", ranges) => loop(todo.tail, accepted :+ ranges)
            case Some(wfl, ranges) =>
              loop(todo.tail ++ self(wfl).evaluateRanges(ranges), accepted)

        loop(
          Vector("in" -> Ranges(1 to 4000, 1 to 4000, 1 to 4000, 1 to 4000)),
          Vector.empty[Ranges]
        )

  lazy val pt1: Int =
    import Part.*

    parts
      .acceptedParts
      .map(_.totalRating)
      .sum

  lazy val pt2: Long =
    import Ranges.*

    workflows
      .acceptedRanges
      .map(_.combinations)
      .sum

  answer(1)(pt1)

  answer(2)(pt2)
