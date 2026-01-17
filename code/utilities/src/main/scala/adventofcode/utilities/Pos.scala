package adventofcode
package utilities

case class Pos(x: Int, y: Int):
  @annotation.targetName("addition")
  def +(that: Pos): Pos =
    Pos(x + that.x, y + that.y)

  @annotation.targetName("subtraction")
  def -(that: Pos): Pos =
    Pos(x - that.x, y - that.y)

  @annotation.targetName("product")
  def *(i: Int): Pos =
    Pos(x * i, y * i)

  def min(that: Pos): Pos =
    Pos(x min that.x, y min that.y)

  def max(that: Pos): Pos =
    Pos(x max that.x, y max that.y)

  def delta(that: Pos): Pos =
    Pos(
      (x max that.x) - (x min that.x),
      (y max that.y) - (y min that.y)
    )

  def sign(that: Pos): Pos =
    Pos((x - that.x).sign, (y - that.y).sign)

  def manhattan(that: Pos): Long = (x - that.x).abs + (y - that.y).abs

  def euclidean(that: Pos): Double =
    math.sqrt:
      math.pow((x - that.x).abs, 2) + math.pow((y - that.y).abs, 2)

  def axisOffsets: Set[Pos] =
    Set(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1), Pos(x, y - 1))

  def diagonalOffsets: Set[Pos] =
    Set(
      Pos(x - 1, y + 1),
      Pos(x + 1, y + 1),
      Pos(x - 1, y - 1),
      Pos(x + 1, y - 1)
    )

  def allOffsets: Set[Pos] =
    axisOffsets ++ diagonalOffsets

  def axisOffsetsFn(f: Pos => Boolean): Set[Pos] =
    axisOffsets.filter(f)

  def diagonalOffsetsFn(f: Pos => Boolean): Set[Pos] =
    diagonalOffsets.filter(f)

  def allOffsetsFn(f: Pos => Boolean): Set[Pos] =
    allOffsets.filter(f)

object Pos:
  def zero: Pos =
    Pos(0, 0)

  def asString(seq: Seq[Pos], found: Char = '#', notFound: Char = '.'): String =
    def intString(i: Int): String =
      val int: String = i.abs.toString

      if int.length == 1 then "0" + int else int

    val box: Box              = Box.bounding(seq)
    val Pos(dx: Int, dy: Int) = box.delta

    val legend10: String = "\n   " + (box.min.x - 1 to box.max.x + 1)
      .map(intString(_).dropRight(1).last)
      .mkString(" ") + "\n"

    val legend1: String = "   " + (box.min.x - 1 to box.max.x + 1)
      .map(intString(_).last)
      .mkString(" ") + "\n"

    val rowFirst: String =
      s"${intString(box.min.y - 1).takeRight(2)} " +
        (s"$notFound " * (dx + 3)).trim + "\n"

    val rowLast: String =
      s"${intString(box.max.y + 1).takeRight(2)} " +
        (s"$notFound " * (dx + 3)).trim + "\n"

    val body: String =
      box.iterator.map: (p: Pos) =>
        if seq.contains(p) then found else notFound
      .grouped(dx + 1)
        .zip(box.min.y to box.max.y)
        .map: (r, i) =>
          s"${intString(i).takeRight(2)} " +
            r.mkString(s"$notFound ", " ", s" $notFound") + "\n"
        .mkString

    legend10 + legend1 + rowFirst + body + rowLast

  extension (self: Seq[Pos])
    def shoelaceFormula: Long = (self.last +: self :+ self.head)
      .sliding(3)
      .map:
        case Seq(a: Pos, b: Pos, c: Pos) =>
          b.x.toLong * (c.y.toLong - a.y.toLong)
      .sum
      .abs
      / 2
