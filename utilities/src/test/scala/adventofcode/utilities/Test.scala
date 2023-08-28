package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Box"):
		val box1 = Box(Pos(0, -2), Pos(2, 0))
		val box2 = Box(Pos(1, -1), Pos(3, 3))
		val pos  = Set(
			Pos(0, -2), Pos(0, -1), Pos(0, 0),
			Pos(1, -2), Pos(1, -1), Pos(1, 0),
			Pos(2, -2), Pos(2, -1), Pos(2, 0),
		)

		assertResult(Pos(2, 2))(box1.delta)
		assertResult(4)(box1.area)

		assertResult(Box(Pos(0, -2), Pos(3, 3)))(box1.union(box2))

		assertResult(true)(box1.contains(Pos(1, 0)))
		assertResult(false)(box2.contains(Pos(1, -2)))

		assertResult(pos)(box1.iterator.toSet)

		assertResult(box1)(Box.bounding(pos))

	test("Box3D"):
		val box1 = Box3D(Pos3D(0, 1, -2), Pos3D(2, 1, 3))
		val box2 = Box3D(Pos3D(-1, -1, 1), Pos3D(3, 3, 3))
		val pos  = Set(
			Pos3D(x = 0, y = 1, z = -2), Pos3D(x = 0, y = 1, z = -1), Pos3D(x = 0, y = 1, z = 0),
			Pos3D(x = 0, y = 1, z = 1), Pos3D(x = 0, y = 1, z = 2), Pos3D(x = 0, y = 1, z = 3),
			Pos3D(x = 1, y = 1, z = -2), Pos3D(x = 1, y = 1, z = -1), Pos3D(x = 1, y = 1, z = 0),
			Pos3D(x = 1, y = 1, z = 1), Pos3D(x = 1, y = 1, z = 2), Pos3D(x = 1, y = 1, z = 3),
			Pos3D(x = 2, y = 1, z = -2), Pos3D(x = 2, y = 1, z = -1), Pos3D(x = 2, y = 1, z = 0),
			Pos3D(x = 2, y = 1, z = 1), Pos3D(x = 2, y = 1, z = 2), Pos3D(x = 2, y = 1, z = 3),
		)

		assertResult(Pos3D(2, 0, 5))(box1.delta)
		assertResult(20)(box1.area)

		assertResult(Box3D(Pos3D(-1, -1, -2), Pos3D(3, 3, 3)))(box1.union(box2))

		assertResult(true)(box1.contains(Pos3D(1, 1, 0)))
		assertResult(false)(box2.contains(Pos3D(-1, -1, 0)))

		assertResult(pos)(box1.iterator.toSet)

		assertResult(box1)(Box3D.bounding(pos))

	test("Box4D"):
		val box1 = Box4D(Pos4D(0, 1, -2, 0), Pos4D(2, 1, 3, 1))
		val box2 = Box4D(Pos4D(-1, -1, 1, -2), Pos4D(3, 3, 3, 3))
		val pos  = Set(
			Pos4D(x = 1, y = 1, z = 3, w = 0), Pos4D(x = 2, y = 1, z = -2, w = 1),
			Pos4D(x = 0, y = 1, z = 3, w = 1), Pos4D(x = 1, y = 1, z = 2, w = 0),
			Pos4D(x = 2, y = 1, z = -1, w = 0), Pos4D(x = 1, y = 1, z = 0, w = 1),
			Pos4D(x = 2, y = 1, z = 2, w = 1), Pos4D(x = 0, y = 1, z = 3, w = 0),
			Pos4D(x = 2, y = 1, z = -1, w = 1), Pos4D(x = 1, y = 1, z = -1, w = 1),
			Pos4D(x = 1, y = 1, z = -1, w = 0), Pos4D(x = 1, y = 1, z = 2, w = 1),
			Pos4D(x = 2, y = 1, z = -2, w = 0), Pos4D(x = 0, y = 1, z = -1, w = 1),
			Pos4D(x = 2, y = 1, z = 1, w = 0), Pos4D(x = 0, y = 1, z = 2, w = 1),
			Pos4D(x = 0, y = 1, z = -2, w = 1), Pos4D(x = 2, y = 1, z = 0, w = 1),
			Pos4D(x = 2, y = 1, z = 0, w = 0), Pos4D(x = 0, y = 1, z = -2, w = 0),
			Pos4D(x = 1, y = 1, z = 1, w = 0), Pos4D(x = 1, y = 1, z = 1, w = 1),
			Pos4D(x = 0, y = 1, z = 0, w = 1), Pos4D(x = 1, y = 1, z = -2, w = 0),
			Pos4D(x = 2, y = 1, z = 1, w = 1), Pos4D(x = 0, y = 1, z = -1, w = 0),
			Pos4D(x = 2, y = 1, z = 3, w = 1), Pos4D(x = 2, y = 1, z = 2, w = 0),
			Pos4D(x = 2, y = 1, z = 3, w = 0), Pos4D(x = 1, y = 1, z = -2, w = 1),
			Pos4D(x = 1, y = 1, z = 0, w = 0), Pos4D(x = 0, y = 1, z = 1, w = 0),
			Pos4D(x = 0, y = 1, z = 1, w = 1), Pos4D(x = 1, y = 1, z = 3, w = 1),
			Pos4D(x = 0, y = 1, z = 2, w = 0), Pos4D(x = 0, y = 1, z = 0, w = 0),
		)



		assertResult(Pos4D(2, 0, 5, 1))(box1.delta)

		assertResult(Box4D(Pos4D(-1, -1, -2, -2), Pos4D(3, 3, 3, 3)))(box1.union(box2))

		assertResult(true)(box1.contains(Pos4D(1, 1, 0, 0)))
		assertResult(false)(box2.contains(Pos4D(1, 0, 0, 0)))

		assertResult(pos)(box1.iterator.toSet)

		assertResult(box1)(Box4D.bounding(pos))

	test("Pos"):
		val pos1 = Pos(0, 4)
		val pos2 = Pos(2, -2)
		val list = List(
			Pos(0, 0), Pos(1, 0), Pos(2, 0),
			Pos(0, 1), Pos(1, 1), Pos(2, 1),
			Pos(0, 2), Pos(1, 2), Pos(2, 2)
		)

		assertResult(Pos(2, 2))(pos1 + pos2)
		assertResult(Pos(-2, 6))(pos1 - pos2)
		assertResult(Pos(4, -4))(pos2 * 2)

		assertResult(Pos(0, -2))(pos1 min pos2)
		assertResult(Pos(2, 4))(pos1 max pos2)

		assertResult(Pos(2, 6))(pos1 delta pos2)

		assertResult(8)(pos1 manhattan pos2)
		assertResult(8)(pos2 manhattan pos1)

		assertResult(6.324555320336759)(pos1 euclidean pos2)
		assertResult(6.324555320336759)(pos2 euclidean pos1)

		assertResult(Set(Pos(1, -2), Pos(3, -2), Pos(2, -1), Pos(2, -3)))(pos2.axisOffsets)
		assertResult(Set(Pos(1, -1), Pos(3, -1), Pos(1, -3), Pos(3, -3)))(pos2.diagonalOffsets)
		assertResult(Set(Pos(1, -2), Pos(3, -2), Pos(2, -1), Pos(2, -3), Pos(1, -1), Pos(3, -1), Pos(1, -3), Pos(3, -3)))(pos2.allOffsets)

		assertResult(Set(Pos(3, -2), Pos(2, -1), Pos(2, -3)))(pos2.axisOffsetsFn(p => p.x > 1))
		assertResult(Set(Pos(3, -1), Pos(3, -3)))(pos2.diagonalOffsetsFn(p => p.x > 1))
		assertResult(Set(Pos(3, -2), Pos(2, -1), Pos(2, -3), Pos(3, -1), Pos(3, -3)))(pos2.allOffsetsFn(p => p.x > 1))

		assertResult(Pos(0, 0))(Pos.zero)

		assertResult(
			"""
				|  0 1 2
				|0 . . .
				|1 . # .
				|2 . . .
				|""".stripMargin)(Pos.asString(list.filterNot(_ == Pos(1, 1))))

	test("Pos3D"):
		val pos1 = Pos3D(0, 4, -2)
		val pos2 = Pos3D(2, -2, 2)

		assertResult(Pos3D(2, 2, 0))(pos1 + pos2)
		assertResult(Pos3D(-2, 6, -4))(pos1 - pos2)
		assertResult(Pos3D(4, -4, 4))(pos2 * 2)

		assertResult(Pos3D(0, -2, -2))(pos1 min pos2)
		assertResult(Pos3D(2, 4, 2))(pos1 max pos2)

		assertResult(12)(pos1 manhattan pos2)
		assertResult(12)(pos2 manhattan pos1)

		val axisOffsets = Set(
			Pos3D(x = 2, y = -2, z = 1), Pos3D(x = 2, y = -1, z = 2), Pos3D(x = 3, y = -2, z = 2),
			Pos3D(x = 2, y = -3, z = 2), Pos3D(x = 2, y = -2, z = 3), Pos3D(x = 1, y = -2, z = 2)
		)

		val diagonalOffsets = Set(
			Pos3D(x = 3, y = -3, z = 2), Pos3D(x = 3, y = -1, z = 1), Pos3D(x = 2, y = -3, z = 1),
			Pos3D(x = 3, y = -2, z = 3), Pos3D(x = 3, y = -2, z = 1), Pos3D(x = 3, y = -3, z = 1),
			Pos3D(x = 1, y = -1, z = 1), Pos3D(x = 3, y = -1, z = 2), Pos3D(x = 1, y = -1, z = 2),
			Pos3D(x = 1, y = -1, z = 3), Pos3D(x = 1, y = -3, z = 3), Pos3D(x = 1, y = -2, z = 1),
			Pos3D(x = 1, y = -3, z = 1), Pos3D(x = 2, y = -1, z = 3), Pos3D(x = 3, y = -3, z = 3),
			Pos3D(x = 1, y = -3, z = 2), Pos3D(x = 2, y = -3, z = 3), Pos3D(x = 2, y = -1, z = 1),
			Pos3D(x = 3, y = -1, z = 3), Pos3D(x = 1, y = -2, z = 3)
		)

		val axisOffsetsFn = Set(
			Pos3D(x = 2, y = -2, z = 1), Pos3D(x = 2, y = -1, z = 2), Pos3D(x = 3, y = -2, z = 2),
			Pos3D(x = 2, y = -3, z = 2), Pos3D(x = 2, y = -2, z = 3)
		)

		val diagonalOffsetsFn = Set(
			Pos3D(x = 3, y = -3, z = 2), Pos3D(x = 3, y = -1, z = 1), Pos3D(x = 2, y = -3, z = 1),
			Pos3D(x = 3, y = -2, z = 3), Pos3D(x = 3, y = -2, z = 1), Pos3D(x = 3, y = -3, z = 1),
			Pos3D(x = 3, y = -1, z = 2), Pos3D(x = 2, y = -1, z = 3), Pos3D(x = 3, y = -3, z = 3),
			Pos3D(x = 2, y = -3, z = 3), Pos3D(x = 2, y = -1, z = 1), Pos3D(x = 3, y = -1, z = 3)
		)

		assertResult(axisOffsets)(pos2.axisOffsets)
		assertResult(diagonalOffsets)(pos2.diagonalOffsets)
		assertResult(axisOffsets ++ diagonalOffsets)(pos2.allOffsets)

		assertResult(axisOffsetsFn)(pos2.axisOffsetsFn(p => p.x > 1))
		assertResult(diagonalOffsetsFn)(pos2.diagonalOffsetsFn(p => p.x > 1))
		assertResult(axisOffsetsFn ++ diagonalOffsetsFn)(pos2.allOffsetsFn(p => p.x > 1))

		assertResult(Pos3D(0, 0, 0))(Pos3D.zero)

	test("Pos4D"):
		val pos1 = Pos4D(0, 4, -2, 1)
		val pos2 = Pos4D(2, -2, 2, -2)

		assertResult(Pos4D(2, 2, 0, -1))(pos1 + pos2)
		assertResult(Pos4D(-2, 6, -4, 3))(pos1 - pos2)
		assertResult(Pos4D(4, -4, 4, -4))(pos2 * 2)

		assertResult(Pos4D(0, -2, -2, -2))(pos1 min pos2)
		assertResult(Pos4D(2, 4, 2, 1))(pos1 max pos2)

		assertResult(15)(pos1 manhattan pos2)
		assertResult(15)(pos2 manhattan pos1)

		val axisOffsets = Set(
			Pos4D(x = 2, y = -1, z = 2, w = -2), Pos4D(x = 2, y = -2, z = 2, w = -1),
			Pos4D(x = 2, y = -2, z = 2, w = -3), Pos4D(x = 3, y = -2, z = 2, w = -2),
			Pos4D(x = 1, y = -2, z = 2, w = -2), Pos4D(x = 2, y = -3, z = 2, w = -2),
			Pos4D(x = 2, y = -2, z = 1, w = -2), Pos4D(x = 2, y = -2, z = 3, w = -2)
		)

		val diagonalOffsets = Set(
			Pos4D(x = 2, y = -1, z = 1, w = -3), Pos4D(x = 3, y = -1, z = 3, w = -1),
			Pos4D(x = 3, y = -3, z = 3, w = -3), Pos4D(x = 2, y = -2, z = 3, w = -3),
			Pos4D(x = 3, y = -2, z = 1, w = -2), Pos4D(x = 2, y = -3, z = 2, w = -1),
			Pos4D(x = 1, y = -2, z = 3, w = -1), Pos4D(x = 1, y = -1, z = 1, w = -2),
			Pos4D(x = 2, y = -3, z = 3, w = -1), Pos4D(x = 3, y = -3, z = 1, w = -2),
			Pos4D(x = 3, y = -1, z = 2, w = -1), Pos4D(x = 3, y = -1, z = 3, w = -3),
			Pos4D(x = 1, y = -2, z = 1, w = -2), Pos4D(x = 3, y = -2, z = 1, w = -1),
			Pos4D(x = 2, y = -3, z = 1, w = -1), Pos4D(x = 1, y = -2, z = 1, w = -1),
			Pos4D(x = 3, y = -3, z = 3, w = -2), Pos4D(x = 3, y = -3, z = 1, w = -1),
			Pos4D(x = 1, y = -3, z = 2, w = -3), Pos4D(x = 2, y = -3, z = 1, w = -2),
			Pos4D(x = 3, y = -2, z = 2, w = -3), Pos4D(x = 1, y = -3, z = 2, w = -2),
			Pos4D(x = 1, y = -1, z = 2, w = -2), Pos4D(x = 3, y = -2, z = 3, w = -1),
			Pos4D(x = 1, y = -1, z = 1, w = -1), Pos4D(x = 1, y = -2, z = 2, w = -1),
			Pos4D(x = 3, y = -3, z = 2, w = -3), Pos4D(x = 3, y = -1, z = 1, w = -2),
			Pos4D(x = 1, y = -3, z = 1, w = -1), Pos4D(x = 2, y = -1, z = 3, w = -1),
			Pos4D(x = 3, y = -1, z = 1, w = -1), Pos4D(x = 2, y = -1, z = 1, w = -1),
			Pos4D(x = 1, y = -1, z = 3, w = -2), Pos4D(x = 2, y = -3, z = 3, w = -2),
			Pos4D(x = 2, y = -2, z = 3, w = -1), Pos4D(x = 3, y = -3, z = 2, w = -1),
			Pos4D(x = 3, y = -3, z = 1, w = -3), Pos4D(x = 1, y = -1, z = 3, w = -1),
			Pos4D(x = 2, y = -1, z = 3, w = -2), Pos4D(x = 2, y = -1, z = 2, w = -3),
			Pos4D(x = 3, y = -3, z = 3, w = -1), Pos4D(x = 3, y = -1, z = 1, w = -3),
			Pos4D(x = 1, y = -3, z = 1, w = -2), Pos4D(x = 1, y = -3, z = 1, w = -3),
			Pos4D(x = 1, y = -2, z = 2, w = -3), Pos4D(x = 1, y = -3, z = 3, w = -3),
			Pos4D(x = 1, y = -2, z = 3, w = -3), Pos4D(x = 1, y = -3, z = 3, w = -1),
			Pos4D(x = 3, y = -1, z = 2, w = -2), Pos4D(x = 2, y = -3, z = 3, w = -3),
			Pos4D(x = 2, y = -1, z = 3, w = -3), Pos4D(x = 3, y = -3, z = 2, w = -2),
			Pos4D(x = 2, y = -2, z = 1, w = -3), Pos4D(x = 3, y = -2, z = 3, w = -2),
			Pos4D(x = 1, y = -3, z = 3, w = -2), Pos4D(x = 1, y = -1, z = 3, w = -3),
			Pos4D(x = 3, y = -2, z = 1, w = -3), Pos4D(x = 3, y = -2, z = 2, w = -1),
			Pos4D(x = 2, y = -1, z = 2, w = -1), Pos4D(x = 3, y = -1, z = 2, w = -3),
			Pos4D(x = 2, y = -3, z = 1, w = -3), Pos4D(x = 1, y = -2, z = 3, w = -2),
			Pos4D(x = 1, y = -1, z = 2, w = -3), Pos4D(x = 1, y = -2, z = 1, w = -3),
			Pos4D(x = 1, y = -3, z = 2, w = -1), Pos4D(x = 2, y = -1, z = 1, w = -2),
			Pos4D(x = 3, y = -1, z = 3, w = -2), Pos4D(x = 2, y = -2, z = 1, w = -1),
			Pos4D(x = 1, y = -1, z = 1, w = -3), Pos4D(x = 1, y = -1, z = 2, w = -1),
			Pos4D(x = 2, y = -3, z = 2, w = -3), Pos4D(x = 3, y = -2, z = 3, w = -3)
		)

		val axisOffsetsFn = Set(
			Pos4D(x = 2, y = -1, z = 2, w = -2), Pos4D(x = 2, y = -2, z = 2, w = -1),
			Pos4D(x = 2, y = -2, z = 2, w = -3), Pos4D(x = 3, y = -2, z = 2, w = -2),
			Pos4D(x = 2, y = -3, z = 2, w = -2), Pos4D(x = 2, y = -2, z = 1, w = -2),
			Pos4D(x = 2, y = -2, z = 3, w = -2)
		)

		val diagonalOffsetsFn = Set(
			Pos4D(x = 2, y = -1, z = 1, w = -3), Pos4D(x = 3, y = -1, z = 3, w = -1),
			Pos4D(x = 3, y = -3, z = 3, w = -3), Pos4D(x = 2, y = -2, z = 3, w = -3),
			Pos4D(x = 3, y = -2, z = 1, w = -2), Pos4D(x = 2, y = -3, z = 2, w = -1),
			Pos4D(x = 2, y = -3, z = 3, w = -1), Pos4D(x = 3, y = -3, z = 1, w = -2),
			Pos4D(x = 3, y = -1, z = 2, w = -1), Pos4D(x = 3, y = -1, z = 3, w = -3),
			Pos4D(x = 3, y = -2, z = 1, w = -1), Pos4D(x = 2, y = -3, z = 1, w = -1),
			Pos4D(x = 3, y = -3, z = 3, w = -2), Pos4D(x = 3, y = -3, z = 1, w = -1),
			Pos4D(x = 2, y = -3, z = 1, w = -2), Pos4D(x = 3, y = -2, z = 2, w = -3),
			Pos4D(x = 3, y = -2, z = 3, w = -1), Pos4D(x = 3, y = -3, z = 2, w = -3),
			Pos4D(x = 3, y = -1, z = 1, w = -2), Pos4D(x = 2, y = -1, z = 3, w = -1),
			Pos4D(x = 3, y = -1, z = 1, w = -1), Pos4D(x = 2, y = -1, z = 1, w = -1),
			Pos4D(x = 2, y = -3, z = 3, w = -2), Pos4D(x = 2, y = -2, z = 3, w = -1),
			Pos4D(x = 3, y = -3, z = 2, w = -1), Pos4D(x = 3, y = -3, z = 1, w = -3),
			Pos4D(x = 2, y = -1, z = 3, w = -2), Pos4D(x = 2, y = -1, z = 2, w = -3),
			Pos4D(x = 3, y = -3, z = 3, w = -1), Pos4D(x = 3, y = -1, z = 1, w = -3),
			Pos4D(x = 3, y = -1, z = 2, w = -2), Pos4D(x = 2, y = -3, z = 3, w = -3),
			Pos4D(x = 2, y = -1, z = 3, w = -3), Pos4D(x = 3, y = -3, z = 2, w = -2),
			Pos4D(x = 2, y = -2, z = 1, w = -3), Pos4D(x = 3, y = -2, z = 3, w = -2),
			Pos4D(x = 3, y = -2, z = 1, w = -3), Pos4D(x = 3, y = -2, z = 2, w = -1),
			Pos4D(x = 2, y = -1, z = 2, w = -1), Pos4D(x = 3, y = -1, z = 2, w = -3),
			Pos4D(x = 2, y = -3, z = 1, w = -3), Pos4D(x = 2, y = -1, z = 1, w = -2),
			Pos4D(x = 3, y = -1, z = 3, w = -2), Pos4D(x = 2, y = -2, z = 1, w = -1),
			Pos4D(x = 2, y = -3, z = 2, w = -3), Pos4D(x = 3, y = -2, z = 3, w = -3)
		)

		assertResult(axisOffsets)(pos2.axisOffsets)
		assertResult(diagonalOffsets)(pos2.diagonalOffsets)
		assertResult(axisOffsets ++ diagonalOffsets)(pos2.allOffsets)

		assertResult(axisOffsetsFn)(pos2.axisOffsetsFn(p => p.x > 1))
		assertResult(diagonalOffsetsFn)(pos2.diagonalOffsetsFn(p => p.x > 1))
		assertResult(axisOffsetsFn ++ diagonalOffsetsFn)(pos2.allOffsetsFn(p => p.x > 1))

		assertResult(Pos4D(0, 0, 0, 0))(Pos4D.zero)

	test("Grid"):
		import utilities.Grid
		import utilities.Grid.*

		val grid1: Grid[Int] =
			Grid.unit(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))

		val grid2: Grid[Int] =
			Grid.unit(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 10, 9)))

		val grid3: Grid[Int] =
			Grid.unit(Vector(Vector(2, 3, 4), Vector(5, 6, 7), Vector(8, 9, 10)))

		val grid4: Vector[Int] =
			Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)

		val grid5: Grid[Int] =
			Grid.unit(Vector(Vector(1, 4, 7), Vector(2, 5, 8), Vector(3, 6, 9)))

		val grid1AsString: String =
			"""
				|1 2 3
				|4 5 6
				|7 8 9
				|""".stripMargin

		assertResult(true)(grid1.isInstanceOf[Grid[Int]])
		assertResult(true)(Grid.empty[Int].isInstanceOf[Grid[Int]])

		assertResult(8)(grid1(1)(2))
		assertResult(8)(grid1.get(1, 2))

		assertResult(grid2)(grid1(1, 2)(10))
		assertResult(grid2)(grid1.set(1, 2)(10))

		assertResult(Vector(4, 5, 6))(grid1.getRow(1))
		assertResult(Vector(2, 5, 8))(grid1.getCol(1))

		assertResult(grid3)(grid1.map(_ + 1))

		assertResult(4)(grid1.count(_ % 2 == 0))

		assertResult(true)(grid2.exists(_ > 9))

		assertResult(true)(grid2.contains(10))

		assertResult((3, 3))(grid1.size)

		assertResult(grid4)(grid1.iterator.toVector)

		assertResult(grid5)(grid1.transpose)

		assertResult(grid1AsString)(grid1.asString)

	test("Graph & Graph Traversal"):
		val graphList = List(
			(1, 2), (1, 5), (1, 9),
			(2, 3), (3, 4), (3, 2),
			(4, 1), (5, 6), (5, 8),
			(6, 7), (9, 10), (10, 9),
			(7, 6), (6, 8), (8, 5),
			(5, 1), (9, 2), (8, 3)
		)

		val graph =
			Graph.fromTupleList(graphList)

		import GraphTraversal.*

		assertResult(Vector((1, 1), (2, 1), (5, 1), (9, 1), (3, 2), (6, 5), (8, 5), (10, 9)))(graph.breadthFirstSearch(1)(_ == 10).getOrElse("not found"))
		assertResult(Vector((1, 1), (2, 1), (3, 2), (4, 3), (5, 1), (6, 5), (7, 6), (8, 5), (9, 1), (10, 9)))(graph.depthFirstSearch(1)(_ == 10).getOrElse("not found"))
		assertResult("not found")(graph.breadthFirstSearch(1)(_ == 11).getOrElse("not found"))
		assertResult("not found")(graph.depthFirstSearch(1)(_ == 11).getOrElse("not found"))

		assertResult(List(1, 9, 10))(graph.breadthFirstSearchPathTo(1)(_ == 10).getOrElse("not found"))
		assertResult(List(2))(graph.breadthFirstSearchPathTo(2)(_ == 2).getOrElse("not found"))
		assertResult(List(4, 1, 5, 8))(graph.breadthFirstSearchPathTo(4)(_ == 8).getOrElse("not found"))
		assertResult(List(10, 9, 2, 3))(graph.breadthFirstSearchPathTo(10)(_ == 3).getOrElse("not found"))

	test("Weighted Graph, Dijkstra & AStar"):
		import Dijkstra.*

		val weightedGraph = List(
			('z', 'y', 10),
			('z', 'x', 1),
			('x', 'w', 2),
			('w', 'y', 1),
		)

		val res =
			(Map('z' -> 0, 'x' -> 1, 'w' -> 3, 'y' -> 4), Map('y' -> 'w', 'x' -> 'z', 'w' -> 'x'))

		assertResult(res)(WeightedGraph.fromTupleList(weightedGraph).shortestPathTree('z'))

		assertResult((Map('a' -> 0), Map.empty[Char, Char]))(WeightedGraph.fromTupleList(weightedGraph).shortestPathTree('a'))

		def tree(depth: Int): WeightedGraph[List[Boolean]] =
			WeightedGraph.unit:
				case x if x.size < depth => Map((true :: x) -> 1, (false :: x) -> 2)
				case x if x.size == depth => Map(Nil -> 1)
				case _ => Map.empty

		//val testTree = tree(15).shortestPathTree(List(true))

		import AStar.*
		import Orderings.posReadingOrder

		val aStarStart = Pos(0, 0)
		val aStarTarget = Pos(4, 4)
		val aStarSource = Pos(1, 1)
		val aStarGraphBox = Box(aStarStart, aStarTarget)
		val aStarGraph =
			for
				b <- aStarGraphBox.iterator
				a <- b.axisOffsetsFn(aStarGraphBox.contains)
			yield
				(b, a, 1)

		val heuristic: (Pos, Int) => Double =
			(v, _) => aStarTarget.euclidean(v)

		val aStar = WeightedGraph.fromTupleList(aStarGraph.toList)
		val aStarResult = (
			6,
			List(
				Pos(x = 1, y = 1),
				Pos(x = 2, y = 1),
				Pos(x = 2, y = 2),
				Pos(x = 3, y = 2),
				Pos(x = 3, y = 3),
				Pos(x = 4, y = 3),
				Pos(x = 4, y = 4)
			)
		)

		assertResult(Some(aStarResult))(aStar.shortestPathTo(aStarSource, _ == aStarTarget)(heuristic))
		assertResult(Some(0, List(aStarSource)))(aStar.shortestPathTo(aStarSource, _ == aStarSource)(heuristic))
		assertResult(None)(aStar.shortestPathTo(aStarSource, _ == Pos(5, 5))(heuristic))

		val aStarTestBox = Box(Pos(0, 0), Pos(10, 10))
		val aStarTestSeq = aStarTestBox.iterator.toVector.filter(p => p.x != 5 || p.y == 10)

		val aStarTestGraph =
			for
				b <- aStarTestSeq
				a <- b.axisOffsetsFn(aStarTestSeq.contains)
			yield
				(b, a, 1)

		val aStarTestH: (Pos, Int) => Double =
			(v, _) => math.sqrt(math.pow(9 - v.x, 2) + math.pow(v.y - 1, 2))

		val aStarTest = WeightedGraph.fromTupleList(aStarTestGraph.toList)

		val (aStarDist, aStarRes) = aStarTest.shortestPathTo(Pos(1, 1), _ == Pos(9, 1))(aStarTestH).get

		val (dijkstraEdges, dijkstraTree) = aStarTest.shortestPathTree(Pos(1, 1))

		val dijkstraRes = utilities.GraphTraversal.treeToPath(Pos(9, 1), dijkstraTree, List.empty[Pos])

		//println:
		//	s"AStar: $aStarDist" +
		//		Pos.asString(aStarTestSeq.filterNot(aStarRes.contains))
		//
		//println:
		//	s"Dijkstra: ${dijkstraEdges(Pos(9, 1))}" +
		//		Pos.asString(aStarTestSeq.filterNot(dijkstraRes.contains))

		assertResult(aStarDist)(dijkstraEdges(Pos(9, 1)))

	test("Cycle"):
		val cycle: Int => Int =
			case i if i >= 10 => i % 5 + 5
			case i => i + 1

		assertResult(Cycle(5, 6, 5, 10))(Cycle.find(cycle, 0)(identity))

	test("Utilities"):
		import Utilities.*

		val fn: Int => Int = (a: Int) => a + 10

		assertResult((522, 1034))(exponentialSearch(fn, 10)(750))
		assertResult(740)(binarySearch(fn, 10, 999)(750))
		assertResult(740)(exponentialBinarySearch(fn, 10)(750))

		val rotateSeq = Utilities.rotateSeq(Vector(1, 2, 3, 4, 5))
		val cycleSeq  = Utilities.cycleSeq(Vector.range(1, 11))

		assertResult(4)(cycleSeq(3))
		assertResult(4)(cycleSeq(-7))
		assertResult(4)(cycleSeq(33))

		assertResult(Vector(4, 5, 1, 2, 3))(rotateSeq(3))
		assertResult(Vector(4, 5, 1, 2, 3))(rotateSeq(-7))
		assertResult(Vector(4, 5, 1, 2, 3))(rotateSeq(33))

		assertResult(3)(3 +% 10)
		assertResult(3)(-7 +% 10)
		assertResult(3)(33 +% 10)

	test("JSON"):
		val jsonTxt =
			"""
				|{
				|	"Company name" : "Microsoft Corporation",
				|	"Ticker"  : "MSFT",
				|	"Active"  : true,
				|
				|	"Price"   : 30.66,
				|	"Shares outstanding" : 8.38e9,
				|	"Related companies" :
				|
				|        [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
				|
				|}
				|
				|""".stripMargin

		val malformedJson =
			"""
				|{
				|	"Company name" ; "Microsoft Corporation"
				|}
				|
				|""".stripMargin

		import JSON.*

		val succJson: JSON = JSON.json.run(jsonTxt) match
			case Right(json) => json
			case Left(e) => JString(e.toString)

		assertResult(JSON.json.run(jsonTxt))(JSON.json.run(succJson.asString))
		assertResult(Parser.fail("Could not parse character: ;").run(malformedJson))(JSON.json.run(malformedJson))
