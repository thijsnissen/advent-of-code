package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day24 extends AdventOfCode(Prod):
  val hailstones: Vector[Stone3D] =
    input
      .linesIterator
      .map(Stone3D.fromString)
      .toVector

  enum Axis:
    case X, Y, Z

  case class Stone2D(
    a: BigDecimal,
    b: BigDecimal,
    va: BigDecimal,
    vb: BigDecimal
  ):
    def reframe(dva: Int, dvb: Int): Stone2D =
      copy(va = va - dva, vb = vb - dvb)

    def time(pa: BigDecimal, pb: BigDecimal): BigDecimal =
      if va == 0 then (pb - b) / vb else (pa - a) / va

    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
    def intersection(that: Stone2D): Option[(BigDecimal, BigDecimal)] =
      // Line 1
      val a1: BigDecimal = a
      val b1: BigDecimal = b
      val a2: BigDecimal = a + va
      val b2: BigDecimal = b + vb

      // Line 2
      val a3: BigDecimal = that.a
      val b3: BigDecimal = that.b
      val a4: BigDecimal = that.a + that.va
      val b4: BigDecimal = that.b + that.vb

      val denominator: BigDecimal =
        (a1 - a2) * (b3 - b4) - (b1 - b2) * (a3 - a4)

      Option.when(denominator != 0):
        val pa: BigDecimal =
          ((a1 * b2 - b1 * a2) * (a3 - a4) - (a1 - a2) * (a3 * b4 - b3 * a4)) /
            denominator

        val pb: BigDecimal =
          ((a1 * b2 - b1 * a2) * (b3 - b4) - (b1 - b2) * (a3 * b4 - b3 * a4)) /
            denominator

        pa -> pb

  object Stone2D:
    extension (self: Vector[Stone2D])
      def intersections(windowMin: Long, windowMax: Long): Int =
        self
          .combinations(2)
          .count: (stones: Vector[Stone2D]) =>
            stones(0).intersection(stones(1)) match
              case None => false
              case Some(pa, pb) =>
                pa >= windowMin && pa <= windowMax &&
                pb >= windowMin && pb <= windowMax &&
                stones(0).time(pa, pb) >= 0 && stones(1).time(pa, pb) >= 0

      def rock(velocities: IndexedSeq[(Int, Int)]): Stone2D =
        val stones: Vector[Vector[Stone2D]] =
          self.combinations(2).toVector

        @tailrec def loopVelocities(todo: IndexedSeq[(Int, Int)]): Stone2D =
          val (va, vb) = todo.head

          loopStones(stones, va, vb) match
            case Some(a, b) => Stone2D(a, b, va, vb)
            case None       => loopVelocities(todo.tail)

        @tailrec def loopStones(
          todo: Vector[Vector[Stone2D]],
          va: Int,
          vb: Int,
          found: Option[(BigDecimal, BigDecimal)] = None
        ): Option[(BigDecimal, BigDecimal)] =
          todo.headOption match
            case None => found
            case Some(stones) =>
              val aStone = stones(0).reframe(va, vb)
              val bStone = stones(1).reframe(va, vb)

              (aStone.intersection(bStone), found) match
                case (Some(a, b), None) =>
                  loopStones(todo.tail, va, vb, Some(a, b))
                case (Some(a, b), Some(fa, fb)) if a == fa && b == fb =>
                  loopStones(todo.tail, va, vb, found)
                case (None, _) =>
                  loopStones(todo.tail, va, vb, found)
                case _ => None

        loopVelocities(velocities)

  case class Stone3D(
    x: BigDecimal,
    y: BigDecimal,
    z: BigDecimal,
    vx: BigDecimal,
    vy: BigDecimal,
    vz: BigDecimal
  ):
    def project(axis: Axis): Stone2D =
      axis match
        case Axis.X => Stone2D(y, z, vy, vz)
        case Axis.Y => Stone2D(x, z, vx, vz)
        case Axis.Z => Stone2D(x, y, vx, vy)

  object Stone3D:
    def fromString(s: String): Stone3D =
      s match
        case s"$x, $y, $z @ $vx, $vy, $vz" => Stone3D(
            BigDecimal(x.trim.toLong),
            BigDecimal(y.trim.toLong),
            BigDecimal(z.trim.toLong),
            BigDecimal(vx.trim.toLong),
            BigDecimal(vy.trim.toLong),
            BigDecimal(vz.trim.toLong)
          )

    extension (self: Vector[Stone3D])
      def project(axis: Axis): Vector[Stone2D] =
        self.map(_.project(axis))

  lazy val pt1: Int =
    val windowMin: Long = if getEnv == Test then 7 else 200000000000000L
    val windowMax: Long = if getEnv == Test then 27 else 400000000000000L

    hailstones
      .project(Axis.Z)
      .intersections(windowMin, windowMax)

  lazy val pt2: BigDecimal =
    val range: Int = if getEnv == Test then 5 else 300

    val velocities: IndexedSeq[(Int, Int)] =
      for
        a <- -range to range
        b <- -range to range
      yield (a, b)

    val Stone2D(x1, y1, vx1, vy1) = hailstones.project(Axis.Z).rock(velocities)
    val Stone2D(x2, z1, vx2, vz1) = hailstones.project(Axis.Y).rock(velocities)
    val Stone2D(y2, z2, vy2, vz2) = hailstones.project(Axis.X).rock(velocities)

    assert(x1 == x2 && y1 == y2 && z1 == z2)
    assert(vx1 == vx2 && vy1 == vy2 && vz1 == vz2)

    x1 + y1 + z1

  answer(1)(pt1)

  answer(2)(pt2)
