package adventofcode
package utilities

case class Box(min: Pos, max: Pos):
  val delta: Pos =
    max - min

  val area: Long =
    delta.x * delta.y

  def union(that: Box): Box =
    Box(min.min(that.min), max.max(that.max))

  def contains(p: Pos): Boolean =
    p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y

  def contains(that: Box): Boolean =
    min.x <= that.min.x && max.x >= that.max.x &&
      min.y <= that.min.y && max.y >= that.max.y

  def iterator: Iterator[Pos] =
    for
      y <- (min.y to max.y).iterator
      x <- (min.x to max.x).iterator
    yield Pos(x, y)

  def axisOffsets: Set[Pos] =
    val x: Iterator[Pos] =
      for
        x <- (min.x to max.x).iterator
        y <- Iterator.apply(min.y - 1, max.y + 1)
      yield Pos(x, y)

    val y: Iterator[Pos] =
      for
        x <- Iterator.apply(min.x - 1, max.x + 1)
        y <- (min.y to max.y).iterator
      yield Pos(x, y)

    x.toSet ++ y.toSet

  def diagonalOffsets: Set[Pos] =
    Set(
      Pos(max.x + 1, max.y + 1),
      Pos(min.x - 1, min.y - 1),
      Pos(max.x + 1, min.y - 1),
      Pos(min.x - 1, max.y + 1)
    )

  def allOffsets: Set[Pos] =
    axisOffsets ++ diagonalOffsets

  def axisOffsetsFn(f: Pos => Boolean): Set[Pos] =
    axisOffsets.filter(f)

  def diagonalOffsetsFn(f: Pos => Boolean): Set[Pos] =
    diagonalOffsets.filter(f)

  def allOffsetsFn(f: Pos => Boolean): Set[Pos] =
    allOffsets.filter(f)

object Box:
  def apply(p: Pos): Box = Box(p, p)

  def bounding(ps: IterableOnce[Pos]): Box =
    ps.iterator.map(apply).reduce(_.union(_))
