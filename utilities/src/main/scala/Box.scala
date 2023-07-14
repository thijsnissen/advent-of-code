case class Box(min: Pos, max: Pos):
	val delta: Pos =
		max - min

	val area: Long =
		delta.x * delta.y

	def union(that: Box): Box =
		Box(min min that.min, max max that.max)

	def contains(p: Pos): Boolean =
		p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y

object Box:
	def apply(p: Pos): Box = Box(p, p)

	def bounding(ps: IterableOnce[Pos]): Box =
		ps.iterator.map(apply).reduce(_ union _)
