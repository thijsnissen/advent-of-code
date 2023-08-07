package adventofcode
package aoc2018

import utilities.AdventOfCode

object Day08 extends AdventOfCode:
	given Mode = Mode.Prod

	val myInput: Vector[Long] =
		input
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

	lazy val pt1 =
		parseTree(myInput).licenseNumber

	lazy val pt2 =
		parseTree(myInput).value

	answer(1)(pt1)

	answer(2)(pt2)
