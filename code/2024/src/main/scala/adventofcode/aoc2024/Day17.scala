package adventofcode
package aoc2024

import utilities.AdventOfCode.*

object Day17 extends AdventOfCode(Prod):
  val (register: Register, program: Program) =
    val Array(r, p) = input.split("\n\n")

    (
      Register.make(r.linesIterator.filter(_.nonEmpty).toVector),
      p.split(" ")(1).split(",").map(_.trim.toInt).toVector
    )

  type Program = Vector[Int]

  case class Register(a: Long = 0, b: Long = 0, c: Long = 0, io: String = ""):
    lazy val printLine: String =
      io.map(_.asDigit).mkString(",")

    lazy val combo: Int => Long =
      case o if o <= 3 => o
      case 4           => a
      case 5           => b
      case 6           => c

    def run(p: Program, ip: Int = 0): Register =
      if p.length <= ip then this
      else
        (p(ip), p(ip + 1)) match
          case (0, o) => copy(a = a >> combo(o)).run(p, ip + 2)
          case (1, o) => copy(b = b ^ o).run(p, ip + 2)
          case (2, o) => copy(b = combo(o) & 0b111).run(p, ip + 2)
          case (3, o) => run(p, if a == 0 then ip + 2 else o)
          case (4, _) => copy(b = b ^ c).run(p, ip + 2)
          case (5, o) => copy(io = io + s"${combo(o) & 0b111}").run(p, ip + 2)
          case (6, o) => copy(b = a >> combo(o)).run(p, ip + 2)
          case (7, o) => copy(c = a >> combo(o)).run(p, ip + 2)

  object Register:
    def make(s: Vector[String]): Register =
      val r = s.map:
        case s"Register A: $a" => a.toLong
        case s"Register B: $b" => b.toLong
        case s"Register C: $c" => c.toLong

      Register(r(0), r(1), r(2))

    @tailrec def quine(
      p: Program,
      pi: Int = 1,
      ans: Vector[Long] = Vector(0)
    ): Option[Long] =
      if pi > p.length then ans.headOption
      else
        val nans =
          for
            a <- ans
            i <- 0 until 8
            if Register(a = (a << 3) + i)
              .run(p).printLine == p.takeRight(pi).mkString(",")
          yield (a << 3) + i

        quine(p, pi + 1, nans)

  lazy val pt1: String =
    register
      .run(program)
      .printLine

  lazy val pt2: Long =
    Register
      .quine(program)
      .get

  answer(1)(pt1)

  answer(2)(pt2)

  // var a = 46323429
  // var b = 0
  // var c = 0
  // var io = ""
  //
  // while a != 0 do
  //   b = a % 8
  //   b = b ^ 1
  //   c = a >> b
  //   b = b ^ 5
  //   b = b ^ c
  //   a = a / 8
  //   io += b % 8
