package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class BoxSpec extends AnyFunSuite:
  test("Box"):
    val box1 = Box(Pos(0, -2), Pos(2, 0))
    val box2 = Box(Pos(1, -1), Pos(3, 3))
    val pos = Set(
      Pos(0, -2),
      Pos(0, -1),
      Pos(0, 0),
      Pos(1, -2),
      Pos(1, -1),
      Pos(1, 0),
      Pos(2, -2),
      Pos(2, -1),
      Pos(2, 0)
    )

    assertResult(Pos(2, 2))(box1.delta)
    assertResult(4)(box1.area)

    assertResult(Box(Pos(0, -2), Pos(3, 3)))(box1.union(box2))

    assertResult(true)(box1.contains(Pos(1, 0)))
    assertResult(false)(box2.contains(Pos(1, -2)))

    assertResult(true)(Box(Pos.zero, Pos(4, 4)).contains(Box(
      Pos(1, 1),
      Pos(3, 4)
    )))
    assertResult(false)(box1.contains(box2))

    assertResult(pos)(box1.iterator.toSet)

    assertResult(box1)(Box.bounding(pos))

    val box1AxisOffsets: Set[Pos] = Set(
      Pos(1, -3),
      Pos(-1, -1),
      Pos(0, -3),
      Pos(2, 1),
      Pos(3, 0),
      Pos(0, 1),
      Pos(-1, -2),
      Pos(3, -2),
      Pos(2, -3),
      Pos(3, -1),
      Pos(1, 1),
      Pos(-1, 0)
    )
    val box1DiagonalOffsets: Set[Pos] =
      Set(Pos(3, 1), Pos(-1, -3), Pos(3, -3), Pos(-1, 1))
    val box1AllOffsets: Set[Pos] = box1AxisOffsets ++ box1DiagonalOffsets

    val box1AxisOffsetsFn: Set[Pos]     = Set(Pos(2, 1), Pos(0, 1), Pos(1, 1))
    val box1DiagonalOffsetsFn: Set[Pos] = Set(Pos(3, 1), Pos(-1, 1))
    val box1AllOffsetsFn: Set[Pos] = box1AxisOffsetsFn ++ box1DiagonalOffsetsFn

    val offsetsFn: Pos => Boolean = (p: Pos) => p.y == 1

    assertResult(box1AxisOffsets)(box1.axisOffsets)
    assertResult(box1DiagonalOffsets)(box1.diagonalOffsets)
    assertResult(box1AllOffsets)(box1.allOffsets)

    assertResult(box1AxisOffsetsFn)(box1.axisOffsetsFn(offsetsFn))
    assertResult(box1DiagonalOffsetsFn)(box1.diagonalOffsetsFn(offsetsFn))
    assertResult(box1AllOffsetsFn)(box1.allOffsetsFn(offsetsFn))

  test("Box3D"):
    val box1 = Box3D(Pos3D(0, 1, -2), Pos3D(2, 1, 3))
    val box2 = Box3D(Pos3D(-1, -1, 1), Pos3D(3, 3, 3))
    val pos = Set(
      Pos3D(x = 0, y = 1, z = -2),
      Pos3D(x = 0, y = 1, z = -1),
      Pos3D(x = 0, y = 1, z = 0),
      Pos3D(x = 0, y = 1, z = 1),
      Pos3D(x = 0, y = 1, z = 2),
      Pos3D(x = 0, y = 1, z = 3),
      Pos3D(x = 1, y = 1, z = -2),
      Pos3D(x = 1, y = 1, z = -1),
      Pos3D(x = 1, y = 1, z = 0),
      Pos3D(x = 1, y = 1, z = 1),
      Pos3D(x = 1, y = 1, z = 2),
      Pos3D(x = 1, y = 1, z = 3),
      Pos3D(x = 2, y = 1, z = -2),
      Pos3D(x = 2, y = 1, z = -1),
      Pos3D(x = 2, y = 1, z = 0),
      Pos3D(x = 2, y = 1, z = 1),
      Pos3D(x = 2, y = 1, z = 2),
      Pos3D(x = 2, y = 1, z = 3)
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
    val pos = Set(
      Pos4D(x = 1, y = 1, z = 3, w = 0),
      Pos4D(x = 2, y = 1, z = -2, w = 1),
      Pos4D(x = 0, y = 1, z = 3, w = 1),
      Pos4D(x = 1, y = 1, z = 2, w = 0),
      Pos4D(x = 2, y = 1, z = -1, w = 0),
      Pos4D(x = 1, y = 1, z = 0, w = 1),
      Pos4D(x = 2, y = 1, z = 2, w = 1),
      Pos4D(x = 0, y = 1, z = 3, w = 0),
      Pos4D(x = 2, y = 1, z = -1, w = 1),
      Pos4D(x = 1, y = 1, z = -1, w = 1),
      Pos4D(x = 1, y = 1, z = -1, w = 0),
      Pos4D(x = 1, y = 1, z = 2, w = 1),
      Pos4D(x = 2, y = 1, z = -2, w = 0),
      Pos4D(x = 0, y = 1, z = -1, w = 1),
      Pos4D(x = 2, y = 1, z = 1, w = 0),
      Pos4D(x = 0, y = 1, z = 2, w = 1),
      Pos4D(x = 0, y = 1, z = -2, w = 1),
      Pos4D(x = 2, y = 1, z = 0, w = 1),
      Pos4D(x = 2, y = 1, z = 0, w = 0),
      Pos4D(x = 0, y = 1, z = -2, w = 0),
      Pos4D(x = 1, y = 1, z = 1, w = 0),
      Pos4D(x = 1, y = 1, z = 1, w = 1),
      Pos4D(x = 0, y = 1, z = 0, w = 1),
      Pos4D(x = 1, y = 1, z = -2, w = 0),
      Pos4D(x = 2, y = 1, z = 1, w = 1),
      Pos4D(x = 0, y = 1, z = -1, w = 0),
      Pos4D(x = 2, y = 1, z = 3, w = 1),
      Pos4D(x = 2, y = 1, z = 2, w = 0),
      Pos4D(x = 2, y = 1, z = 3, w = 0),
      Pos4D(x = 1, y = 1, z = -2, w = 1),
      Pos4D(x = 1, y = 1, z = 0, w = 0),
      Pos4D(x = 0, y = 1, z = 1, w = 0),
      Pos4D(x = 0, y = 1, z = 1, w = 1),
      Pos4D(x = 1, y = 1, z = 3, w = 1),
      Pos4D(x = 0, y = 1, z = 2, w = 0),
      Pos4D(x = 0, y = 1, z = 0, w = 0)
    )

    assertResult(Pos4D(2, 0, 5, 1))(box1.delta)

    assertResult(Box4D(Pos4D(-1, -1, -2, -2), Pos4D(3, 3, 3, 3)))(
      box1.union(box2)
    )

    assertResult(true)(box1.contains(Pos4D(1, 1, 0, 0)))
    assertResult(false)(box2.contains(Pos4D(1, 0, 0, 0)))

    assertResult(pos)(box1.iterator.toSet)

    assertResult(box1)(Box4D.bounding(pos))
