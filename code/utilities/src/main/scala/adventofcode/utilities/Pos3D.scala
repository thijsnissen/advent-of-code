package adventofcode
package utilities

case class Pos3D(x: Int, y: Int, z: Int):
  @annotation.targetName("addition")
  def +(that: Pos3D): Pos3D =
    Pos3D(x + that.x, y + that.y, z + that.z)

  @annotation.targetName("addition")
  def +(i: Int): Pos3D =
    Pos3D(x + i, y + i, z + i)

  @annotation.targetName("subtraction")
  def -(that: Pos3D): Pos3D =
    Pos3D(x - that.x, y - that.y, z - that.z)

  @annotation.targetName("subtraction")
  def -(i: Int): Pos3D =
    Pos3D(x - i, y - i, z - i)

  @annotation.targetName("product")
  def *(i: Int): Pos3D =
    Pos3D(x * i, y * i, z * i)

  @annotation.targetName("division")
  def /(i: Int): Pos3D =
    Pos3D(x / i, y / i, z / i)

  infix def min(that: Pos3D): Pos3D =
    Pos3D(x min that.x, y min that.y, z min that.z)

  infix def max(that: Pos3D): Pos3D =
    Pos3D(x max that.x, y max that.y, z max that.z)

  def manhattan(that: Pos3D): Long = (x - that.x).abs + (y - that.y).abs +
    (z - that.z).abs

  def axisOffsets: Set[Pos3D] =
    Set(
      Pos3D(x + 1, y, z),
      Pos3D(x - 1, y, z),
      Pos3D(x, y + 1, z),
      Pos3D(x, y - 1, z),
      Pos3D(x, y, z - 1),
      Pos3D(x, y, z + 1)
    )

  def diagonalOffsets: Set[Pos3D] =
    allOffsets -- axisOffsets

  def allOffsets: Set[Pos3D] =
    (for
      x <- x - 1 to x + 1
      y <- y - 1 to y + 1
      z <- z - 1 to z + 1
      if Pos3D(x, y, z) != this
    yield Pos3D(x, y, z)).toSet

  def axisOffsetsFn(f: Pos3D => Boolean): Set[Pos3D] =
    axisOffsets.filter(f)

  def diagonalOffsetsFn(f: Pos3D => Boolean): Set[Pos3D] =
    diagonalOffsets.filter(f)

  def allOffsetsFn(f: Pos3D => Boolean): Set[Pos3D] =
    allOffsets.filter(f)

object Pos3D:
  def zero: Pos3D =
    Pos3D(0, 0, 0)

  def apply(i: Int): Pos3D =
    Pos3D(i, i, i)
