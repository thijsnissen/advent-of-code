package adventofcode
package aoc2024

import utilities.AdventOfCode.*

object Day24 extends AdventOfCode(Prod):
  val (wires: Wires, gates: Vector[Gate]) =
    val Array(w, g) = input.split("\n\n")

    (
      Wires.fromString(w),
      g.linesIterator.map(Gate.fromString).toVector
    )

  type Wires = Map[String, Int]

  object Wires:
    def fromString(s: String): Wires =
      s
        .linesIterator
        .map:
          case s"$a: $b" => (a, b.toInt)
        .toMap

    def asString(w: Char, b: Int): String =
      s"$w" + "%02d".format(b)

    extension (self: Wires)
      def number(w: Char): Long =
        self
          .toVector
          .filter((k, _) => k.startsWith(s"$w"))
          .sortBy((k, _) => -k.tail.toInt)
          .map((_, v) => v)
          .mkString
          .foldLeft(0L): (acc, b) =>
            (acc << 1) + b.asDigit

  enum Gate:
    val l: String
    val r: String
    val o: String

    case AND(l: String, r: String, o: String)
    case XOR(l: String, r: String, o: String)
    case OR(l: String, r: String, o: String)

    def copy(nl: String = l, nr: String = r, no: String = o): Gate =
      this match
        case _: AND => AND(nl, nr, no)
        case _: XOR => XOR(nl, nr, no)
        case _: OR  => OR(nl, nr, no)

    def exec(ws: Wires): Int =
      this match
        case AND(l, r, _) => ws(l) & ws(r)
        case XOR(l, r, _) => ws(l) ^ ws(r)
        case OR(l, r, _)  => ws(l) | ws(r)

    def hasInputWires(a: String, b: String): Boolean =
      Set(l, r) == Set(a, b)

    def swap(that: Gate): Vector[Gate] =
      Vector(this.copy(no = that.o), that.copy(no = o))

  object Gate:
    def fromString(s: String): Gate =
      s match
        case s"$l $op $r -> $o" => op match
            case "AND" => AND(l, r, o)
            case "XOR" => XOR(l, r, o)
            case "OR"  => OR(l, r, o)

    extension (self: Vector[Gate])
      def run(ws: Wires): Wires =
        @tailrec def loop(todo: Vector[Gate], acc: Wires): Wires =
          todo.headOption match
            case None                         => acc
            case Some(g) if acc.contains(g.o) => loop(todo.tail, acc)
            case Some(g) if acc.contains(g.l) && acc.contains(g.r) =>
              loop(todo.tail, acc.updated(g.o, g.exec(acc)))
            case Some(g) =>
              loop(
                self.find(_.o == g.l).get +: self.find(_.o == g.r).get +: todo,
                acc
              )

        loop(self, ws)

      def swaps: Vector[String] =
        @tailrec def loop(
          gs: Map[String, Gate],
          cs: Iterator[Vector[Gate]],
          acc: Vector[String],
          b: Int = 0
        ): Vector[String] =
          if acc.length == 8 then acc
          else
            val Vector(ga, gb)   = cs.next()
            val Vector(nga, ngb) = ga.swap(gb)

            val ngs = gs.updated(nga.o, nga).updated(ngb.o, ngb)
            val nb  = ngs.verify()

            if nb > b
            then
              println(s"swapped wires: ${ga.o} <-> ${gb.o}")
              loop(ngs, self.combinations(2), acc :+ ga.o :+ gb.o, nb)
            else loop(gs, cs, acc, b)

        val gs: Map[String, Gate] =
          self.groupBy(_.o).view.mapValues(_.head).toMap

        loop(
          gs = gs,
          cs = self.combinations(2),
          acc = Vector.empty[String],
          b = gs.verify()
        )

    // Automated solution credited to @hyperneutrino
    // (Only works on ripple-carry adder)
    // see: https://www.youtube.com/watch?v=SU6lp6wyd3I
    extension (self: Map[String, Gate])
      @tailrec def verify(b: Int = 0): Int =
        if self.verifyZ(Wires.asString('z', b), b) then verify(b + 1) else b

      def verifyZ(w: String, b: Int): Boolean =
        self.get(w) match
          case Some(g) if b == 0 => g.hasInputWires("x00", "y00")
          case Some(XOR(l, r, _)) =>
            isIntermediateXor(l, b) && isCarry(r, b) ||
            isIntermediateXor(r, b) && isCarry(l, b)
          case _ => false

      def isIntermediateXor(w: String, b: Int): Boolean =
        self.get(w) match
          case Some(g: XOR) =>
            g.hasInputWires(Wires.asString('x', b), Wires.asString('y', b))
          case _ => false

      def isCarry(w: String, b: Int): Boolean =
        self.get(w) match
          case Some(g: AND) if b == 1 => g.hasInputWires("x00", "y00")
          case Some(OR(l, r, _)) =>
            isDirectCarry(l, b - 1) && isRecarry(r, b - 1) ||
            isDirectCarry(r, b - 1) && isRecarry(l, b - 1)
          case _ => false

      def isDirectCarry(w: String, b: Int): Boolean =
        self.get(w) match
          case Some(g: AND) =>
            g.hasInputWires(Wires.asString('x', b), Wires.asString('y', b))
          case _ => false

      def isRecarry(w: String, b: Int): Boolean =
        self.get(w) match
          case Some(AND(l, r, _)) =>
            isIntermediateXor(l, b) && isCarry(r, b) ||
            isIntermediateXor(r, b) && isCarry(l, b)
          case _ => false

  import Gate.*
  import Wires.*

  lazy val pt1: Long =
    gates
      .run(wires)
      .number('z')

  lazy val pt2: String =
    // Manual.print()
    if getEnv == Test then ""
    else
      gates
        .swaps
        .sorted
        .mkString(",")

  answer(1)(pt1)

  answer(2)(pt2)

  object Manual:
    // swaps:
    // z16: z16 <-> hmk
    // z20: z20 <-> fhp
    // z27: tpc <-> rvf
    // z33: z33 <-> fcd

    lazy val run: Wires = gates.run(wires)

    def print(): Unit =
      println("n: x y c = z  nc")

      val (e, _) =
        run.number('x').toBinaryString.reverse
          .zip(run.number('y').toBinaryString.reverse)
          .zip(run.number('z').toBinaryString.reverse)
          .zipWithIndex
          .foldLeft(("", 0)):
            case ((acc, c), (((x, y), z), b)) =>
              val nc = if x.asDigit + y.asDigit + c > 1 then 1 else 0

              println("%02d".format(b) + s" $x $y $c = $z  $nc")

              if (x.asDigit + y.asDigit + c) % 2 == z.asDigit then (acc, nc)
              else (if acc.isEmpty then asString('z', b) else acc, nc)

      println(if e.nonEmpty then s"Next error at $e" else "All good!")
