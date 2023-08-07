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
		Pos(math.max(x, that.x) - math.min(x, that.x),
			math.max(y, that.y) - math.min(y, that.y))

	def manhattan(that: Pos): Long =
		math.abs(math.max(x, that.x) - math.min(x, that.x)) +
			math.abs(math.max(y, that.y) - math.min(y, that.y))

	def adjacentHrVr(b: Box): List[Pos] =
		List(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1), Pos(x, y - 1)).filter(b.contains)

	def adjacentDgn(b: Box): List[Pos] =
		List(Pos(x - 1, y + 1), Pos(x + 1, y + 1), Pos(x - 1, y - 1), Pos(x + 1, y - 1)).filter(b.contains)

	def adjacentHrVrFromSeq(s: Seq[Pos]): List[Pos] =
		List(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1), Pos(x, y - 1)).filter(s.contains)

	def adjacentDgnFromSeq(s: Seq[Pos]): List[Pos] =
		List(Pos(x - 1, y + 1), Pos(x + 1, y + 1), Pos(x - 1, y - 1), Pos(x + 1, y - 1)).filter(s.contains)

object Pos:
	def unit: Pos =
		Pos(0, 0)

	def asString(seq: Seq[Pos], found: Char = '.', notFound: Char = '#'): String =
		val xMin   = seq.minBy(_.x)
		val xMax   = seq.maxBy(_.x)
		val yMin   = seq.minBy(_.y)
		val yMax   = seq.maxBy(_.y)
		val deltaX = xMin.delta(xMax).x

		val a =
			for
				y <- (yMin.y to yMax.y).iterator
				x <- (xMin.x to xMax.x).iterator
			yield
				if seq.contains(Pos(x, y)) then found.toString + " " else notFound.toString + " "

		val b =
			a
				.zipWithIndex
				.map:
					(s, i) => if (i + 1) % (deltaX + 1) == 0 then s.trim + "\n" else s

		val xLegend = "  " +
			(xMin.x to xMax.x)
				.iterator
				.map(_.toString.last + " ")
				.mkString

		val yLegend =
			(yMin.y to yMax.y).iterator

		val result =
			(xLegend.init + "\n" + b.mkString).map:
				c =>
					if c == '\n' && yLegend.hasNext then
						c.toString + yLegend.next.toString.last + " "
					else
						c.toString

		result.mkString("\n", "", "")

	given posReadingOrder: Ordering[Pos] =
		Ordering.fromLessThan:
			(a, b) => a.y < b.y || (a.y == b.y && a.x < b.x)