package adventofcode
package utilities

case class Box3D(min: Pos3D, max: Pos3D):
  val delta: Pos3D =
    max - min

  val area: Long =
    2 * (delta.x * delta.y + delta.z * delta.x + delta.z * delta.y)

  def union(that: Box3D): Box3D =
    Box3D(min.min(that.min), max.max(that.max))

  def contains(p: Pos3D): Boolean =
    p.x >= min.x && p.x <= max.x &&
      p.y >= min.y && p.y <= max.y &&
      p.z >= min.z && p.z <= max.z

  def manhattan(that: Pos3D): Int =
    (if that.x >= min.x && that.x <= max.x then 0
     else math.min(math.abs(min.x - that.x), math.abs(max.x - that.x))) +
      (if that.y >= min.y && that.y <= max.y then 0
       else math.min(math.abs(min.y - that.y), math.abs(max.y - that.y))) +
      (if that.z >= min.z && that.z <= max.z then 0
       else math.min(math.abs(min.z - that.z), math.abs(max.z - that.z)))

  def iterator: Iterator[Pos3D] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
    yield Pos3D(x, y, z)

object Box3D:
  def apply(p: Pos3D): Box3D = Box3D(p, p)

  def bounding(ps: IterableOnce[Pos3D]): Box3D =
    ps.iterator.map(apply).reduce(_.union(_))
