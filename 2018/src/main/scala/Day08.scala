import scala.io.Source

object Day08 extends App:
	val day: String =
		this.getClass.getName.dropRight(1).toLowerCase

	val input: Vector[Long] =
		Source
			.fromResource(s"$day-input.txt")
			.getLines
			.mkString
			.split(" ")
			.map(_.toLong)
			.toVector

	case class Node(children: Vector[Node], meta: Vector[Long]):
		def licenseNumber: Long =
			meta.sum + children.map(_.licenseNumber).sum

		def value: Long =
			if children.isEmpty then
				meta.sum
			else
				meta.map(i => children.lift(i.toInt - 1).map(_.value).getOrElse(0L)).sum

	def parseTree(input: Vector[Long]): Node =
		def go(
			input: Vector[Long],
			childCount: Long,
			metaCount: Long,
			children: Vector[Node] = Vector.empty[Node]
		): (Node, Vector[Long]) =
			if childCount == 0 then
				(Node(children, input.take(metaCount.toInt)), input.drop(metaCount.toInt))
			else
				val (child, restOfInput) =
					go(input.drop(2), input.head, input.drop(1).head, Vector.empty[Node])

				go(restOfInput, childCount - 1, metaCount, children :+ child)

		go(input.drop(2), input.head, input.drop(1).head)._1

	val startTimePart1: Long =
		System.currentTimeMillis

	val answerPart1 =
		parseTree(input).licenseNumber

	// test: 138 [0ms], input: 37439 [2ms]
	println(s"The answer to $day part 1 is: $answerPart1 [${System.currentTimeMillis - startTimePart1}ms]")

	val startTimePart2: Long =
		System.currentTimeMillis

	val answerPart2 =
		parseTree(input).value

	// test: 66 [1ms], input: 20815 [1ms]
	println(s"The answer to $day part 2 is: $answerPart2 [${System.currentTimeMillis - startTimePart2}ms]")
