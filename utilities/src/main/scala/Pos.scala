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

	def manhattan(that: Pos): Long =
		math.abs(math.abs(x) - math.abs(that.x)) +
			math.abs(math.abs(y) - math.abs(that.y))

	def adjacentHrVr(b: Box): List[Pos] =
		List(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1), Pos(x, y - 1)).filter(b.contains)

	def adjacentDgn(b: Box): List[Pos] =
		List(Pos(x - 1, y + 1), Pos(x + 1, y + 1), Pos(x - 1, y - 1), Pos(x + 1, y - 1)).filter(b.contains)
