package adventofcode
package utilities

case class Box4D(min: Pos4D, max: Pos4D):
  val delta: Pos4D =
    max - min

  def union(that: Box4D): Box4D =
    Box4D(min.min(that.min), max.max(that.max))

  def contains(p: Pos4D): Boolean =
    p.x >= min.x && p.x <= max.x &&
      p.y >= min.y && p.y <= max.y &&
      p.z >= min.z && p.z <= max.z &&
      p.w >= min.w && p.w <= max.w

  def iterator: Iterator[Pos4D] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
      w <- (min.w to max.w).iterator
    yield Pos4D(x, y, z, w)

object Box4D:
  def apply(p: Pos4D): Box4D = Box4D(p, p)

  def bounding(ps: IterableOnce[Pos4D]): Box4D =
    ps.iterator.map(apply).reduce(_.union(_))
