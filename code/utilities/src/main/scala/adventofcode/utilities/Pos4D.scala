package adventofcode
package utilities

case class Pos4D(x: Int, y: Int, z: Int, w: Int):
  @annotation.targetName("addition")
  def +(that: Pos4D): Pos4D =
    Pos4D(x + that.x, y + that.y, z + that.z, w + that.w)

  @annotation.targetName("subtraction")
  def -(that: Pos4D): Pos4D =
    Pos4D(x - that.x, y - that.y, z - that.z, w - that.w)

  @annotation.targetName("product")
  def *(i: Int): Pos4D =
    Pos4D(x * i, y * i, z * i, w * i)

  def min(that: Pos4D): Pos4D =
    Pos4D(x min that.x, y min that.y, z min that.z, w min that.w)

  def max(that: Pos4D): Pos4D =
    Pos4D(x max that.x, y max that.y, z max that.z, w max that.w)

  def manhattan(that: Pos4D): Long =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs

  def axisOffsets: Set[Pos4D] =
    Set(
      Pos4D(x + 1, y, z, w),
      Pos4D(x - 1, y, z, w),
      Pos4D(x, y + 1, z, w),
      Pos4D(x, y - 1, z, w),
      Pos4D(x, y, z - 1, w),
      Pos4D(x, y, z + 1, w),
      Pos4D(x, y, z, w - 1),
      Pos4D(x, y, z, w + 1)
    )

  def diagonalOffsets: Set[Pos4D] =
    allOffsets -- axisOffsets

  def allOffsets: Set[Pos4D] =
    (for
      x <- x - 1 to x + 1
      y <- y - 1 to y + 1
      z <- z - 1 to z + 1
      w <- w - 1 to w + 1
      if Pos4D(x, y, z, w) != this
    yield Pos4D(x, y, z, w)).toSet

  def axisOffsetsFn(f: Pos4D => Boolean): Set[Pos4D] =
    axisOffsets.filter(f)

  def diagonalOffsetsFn(f: Pos4D => Boolean): Set[Pos4D] =
    diagonalOffsets.filter(f)

  def allOffsetsFn(f: Pos4D => Boolean): Set[Pos4D] =
    allOffsets.filter(f)

object Pos4D:
  def zero: Pos4D =
    Pos4D(0, 0, 0, 0)
