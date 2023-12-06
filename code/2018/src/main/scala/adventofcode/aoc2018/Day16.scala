package adventofcode
package aoc2018

import utilities.AdventOfCode.*

object Day16 extends AdventOfCode(Prod):
  import scala.util.matching.Regex

  val samples: List[Sample] =
    Sample
      .sampleRegex
      .findAllIn(input)
      .collect(s => Sample.fromString(s))
      .toList

  val instructions: List[Instruction] =
    Sample
      .sampleRegex
      .replaceAllIn(input, "")
      .linesIterator
      .collect:
        case s"$i0 $i1 $i2 $i3" =>
          Instruction(i0.toInt, i1.toInt, i2.toInt, i3.toInt)
      .toList

  case class Sample(inst: Instruction, before: Device, after: Device):
    def opCode: Int =
      inst.opcode

  object Sample:
    val sampleRegex: Regex =
      """Before: \[(\d+), (\d+), (\d+), (\d+)]
				|(\d+) (\d+) (\d+) (\d+)
				|After:  \[(\d+), (\d+), (\d+), (\d+)]""".stripMargin.r

    def fromString(s: String): Sample =
      s match
        case sampleRegex(b0, b1, b2, b3, i0, i1, i2, i3, a0, a1, a2, a3) =>
          Sample(
            Instruction(i0.toInt, i1.toInt, i2.toInt, i3.toInt),
            Device.unit(Map(
              0 -> b0.toInt,
              1 -> b1.toInt,
              2 -> b2.toInt,
              3 -> b3.toInt
            )),
            Device.unit(Map(
              0 -> a0.toInt,
              1 -> a1.toInt,
              2 -> a2.toInt,
              3 -> a3.toInt
            ))
          )

  case class Instruction(opcode: Int, a: Int, b: Int, c: Int)

  opaque type Device =
    Map[Int, Int]

  object Device:
    lazy val unorderedOpcodes: List[String] =
      List(
        "addr",
        "addi",
        "mulr",
        "muli",
        "banr",
        "bani",
        "borr",
        "bori",
        "setr",
        "seti",
        "gtir",
        "gtri",
        "gtrr",
        "eqir",
        "eqri",
        "eqrr"
      )

    def unit(m: Map[Int, Int]): Device =
      assert(
        m.keys == Set(0, 1, 2, 3),
        "The device has four registers (numbered 0 through 3)"
      )

      m.withDefaultValue(0)

    def empty: Device =
      Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0).withDefaultValue(0)

    def analyzeSamplesBySample: List[List[String]] =
      samples.foldLeft(List.empty[List[String]]): (acc, sample) =>
        val result = unorderedOpcodes.map: opcode =>
          if sample.before.run(
              opcode,
              sample.inst.a,
              sample.inst.b,
              sample.inst.c
            ) == sample.after
          then
            opcode
          else
            ""

        result.filter(_.nonEmpty) :: acc

    def analyzeSamplesByOpcode: List[(String, List[Int])] =
      unorderedOpcodes
        .foldLeft(List.empty[(String, List[Int])]): (acc, opcode) =>
          val result = samples.map: sample =>
            if sample.before.run(
                opcode,
                sample.inst.a,
                sample.inst.b,
                sample.inst.c
              ) == sample.after
            then
              sample.opCode
            else
              -1

          (opcode, result.filter(_ >= 0).distinct) :: acc

    @annotation.tailrec
    def orderedOpcodes(
      opcodes: List[(String, List[Int])],
      acc: List[(Int, String)]
    ): List[String] =
      opcodes.sortBy((_, codes) => codes.length) match
        case (name, code) :: t if code.length == 1 =>
          orderedOpcodes(
            t.map((n, c) => (n, c.filterNot(_ == code.head))),
            (code.head, name) :: acc
          )
        case Nil => acc.sortBy((code, _) => code).map((_, name) => name)
        case _   => sys.error("Cannot order opcodes for the given input")

    extension (self: Device)
      def run(opcode: String, a: Int, b: Int, c: Int): Device =
        opcode match
          case "addr" => self.updated(c, self(a) + self(b))
          case "addi" => self.updated(c, self(a) + b)
          case "mulr" => self.updated(c, self(a) * self(b))
          case "muli" => self.updated(c, self(a) * b)
          case "banr" => self.updated(c, self(a) & self(b))
          case "borr" => self.updated(c, self(a) | self(b))
          case "bori" => self.updated(c, self(a) | b)
          case "bani" => self.updated(c, self(a) & b)
          case "setr" => self.updated(c, self(a))
          case "seti" => self.updated(c, a)
          case "gtir" => self.updated(c, if a > self(b) then 1 else 0)
          case "gtri" => self.updated(c, if self(a) > b then 1 else 0)
          case "gtrr" => self.updated(c, if self(a) > self(b) then 1 else 0)
          case "eqir" => self.updated(c, if a == self(b) then 1 else 0)
          case "eqri" => self.updated(c, if self(a) == b then 1 else 0)
          case "eqrr" => self.updated(c, if self(a) == self(b) then 1 else 0)

  import Device.*

  lazy val pt1: Int =
    analyzeSamplesBySample
      .count(_.length >= 3)

  lazy val pt2: Int =
    val opcodes: List[String] =
      orderedOpcodes(analyzeSamplesByOpcode, List.empty[(Int, String)])

    val result =
      instructions.foldLeft(empty): (device, inst) =>
        device.run(opcodes(inst.opcode), inst.a, inst.b, inst.c)

    result(0)

  answer(1)(pt1)

  answer(2)(pt2)
