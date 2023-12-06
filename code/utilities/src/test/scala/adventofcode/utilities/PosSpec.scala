package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class PosSpec extends AnyFunSuite:
  test("Pos"):
    val pos1 = Pos(0, 4)
    val pos2 = Pos(2, -2)
    val list = List(
      Pos(0, 0),
      Pos(1, 0),
      Pos(2, 0),
      Pos(0, 1),
      Pos(1, 1),
      Pos(2, 1),
      Pos(0, 2),
      Pos(1, 2),
      Pos(2, 2)
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

    assertResult(Set(Pos(1, -2), Pos(3, -2), Pos(2, -1), Pos(2, -3)))(
      pos2.axisOffsets
    )
    assertResult(Set(Pos(1, -1), Pos(3, -1), Pos(1, -3), Pos(3, -3)))(
      pos2.diagonalOffsets
    )
    assertResult(Set(
      Pos(1, -2),
      Pos(3, -2),
      Pos(2, -1),
      Pos(2, -3),
      Pos(1, -1),
      Pos(3, -1),
      Pos(1, -3),
      Pos(3, -3)
    ))(pos2.allOffsets)

    assertResult(Set(Pos(3, -2), Pos(2, -1), Pos(2, -3)))(pos2.axisOffsetsFn(
      p => p.x > 1
    ))
    assertResult(Set(Pos(3, -1), Pos(3, -3)))(pos2.diagonalOffsetsFn(
      p =>
        p.x > 1
    ))
    assertResult(Set(
      Pos(3, -2),
      Pos(2, -1),
      Pos(2, -3),
      Pos(3, -1),
      Pos(3, -3)
    ))(pos2.allOffsetsFn(p => p.x > 1))

    assertResult(Pos(0, 0))(Pos.zero)

    assertResult(
      """
				|  0 1 2
				|0 . . .
				|1 . # .
				|2 . . .
				|""".stripMargin
    )(Pos.asString(list.filterNot(_ == Pos(1, 1))))

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
      Pos3D(x = 2, y = -2, z = 1),
      Pos3D(x = 2, y = -1, z = 2),
      Pos3D(x = 3, y = -2, z = 2),
      Pos3D(x = 2, y = -3, z = 2),
      Pos3D(x = 2, y = -2, z = 3),
      Pos3D(x = 1, y = -2, z = 2)
    )

    val diagonalOffsets = Set(
      Pos3D(x = 3, y = -3, z = 2),
      Pos3D(x = 3, y = -1, z = 1),
      Pos3D(x = 2, y = -3, z = 1),
      Pos3D(x = 3, y = -2, z = 3),
      Pos3D(x = 3, y = -2, z = 1),
      Pos3D(x = 3, y = -3, z = 1),
      Pos3D(x = 1, y = -1, z = 1),
      Pos3D(x = 3, y = -1, z = 2),
      Pos3D(x = 1, y = -1, z = 2),
      Pos3D(x = 1, y = -1, z = 3),
      Pos3D(x = 1, y = -3, z = 3),
      Pos3D(x = 1, y = -2, z = 1),
      Pos3D(x = 1, y = -3, z = 1),
      Pos3D(x = 2, y = -1, z = 3),
      Pos3D(x = 3, y = -3, z = 3),
      Pos3D(x = 1, y = -3, z = 2),
      Pos3D(x = 2, y = -3, z = 3),
      Pos3D(x = 2, y = -1, z = 1),
      Pos3D(x = 3, y = -1, z = 3),
      Pos3D(x = 1, y = -2, z = 3)
    )

    val axisOffsetsFn = Set(
      Pos3D(x = 2, y = -2, z = 1),
      Pos3D(x = 2, y = -1, z = 2),
      Pos3D(x = 3, y = -2, z = 2),
      Pos3D(x = 2, y = -3, z = 2),
      Pos3D(x = 2, y = -2, z = 3)
    )

    val diagonalOffsetsFn = Set(
      Pos3D(x = 3, y = -3, z = 2),
      Pos3D(x = 3, y = -1, z = 1),
      Pos3D(x = 2, y = -3, z = 1),
      Pos3D(x = 3, y = -2, z = 3),
      Pos3D(x = 3, y = -2, z = 1),
      Pos3D(x = 3, y = -3, z = 1),
      Pos3D(x = 3, y = -1, z = 2),
      Pos3D(x = 2, y = -1, z = 3),
      Pos3D(x = 3, y = -3, z = 3),
      Pos3D(x = 2, y = -3, z = 3),
      Pos3D(x = 2, y = -1, z = 1),
      Pos3D(x = 3, y = -1, z = 3)
    )

    assertResult(axisOffsets)(pos2.axisOffsets)
    assertResult(diagonalOffsets)(pos2.diagonalOffsets)
    assertResult(axisOffsets ++ diagonalOffsets)(pos2.allOffsets)

    assertResult(axisOffsetsFn)(pos2.axisOffsetsFn(p => p.x > 1))
    assertResult(diagonalOffsetsFn)(pos2.diagonalOffsetsFn(p => p.x > 1))
    assertResult(axisOffsetsFn ++ diagonalOffsetsFn)(pos2.allOffsetsFn(
      p =>
        p.x > 1
    ))

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
      Pos4D(x = 2, y = -1, z = 2, w = -2),
      Pos4D(x = 2, y = -2, z = 2, w = -1),
      Pos4D(x = 2, y = -2, z = 2, w = -3),
      Pos4D(x = 3, y = -2, z = 2, w = -2),
      Pos4D(x = 1, y = -2, z = 2, w = -2),
      Pos4D(x = 2, y = -3, z = 2, w = -2),
      Pos4D(x = 2, y = -2, z = 1, w = -2),
      Pos4D(x = 2, y = -2, z = 3, w = -2)
    )

    val diagonalOffsets = Set(
      Pos4D(x = 2, y = -1, z = 1, w = -3),
      Pos4D(x = 3, y = -1, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 3, w = -3),
      Pos4D(x = 2, y = -2, z = 3, w = -3),
      Pos4D(x = 3, y = -2, z = 1, w = -2),
      Pos4D(x = 2, y = -3, z = 2, w = -1),
      Pos4D(x = 1, y = -2, z = 3, w = -1),
      Pos4D(x = 1, y = -1, z = 1, w = -2),
      Pos4D(x = 2, y = -3, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 1, w = -2),
      Pos4D(x = 3, y = -1, z = 2, w = -1),
      Pos4D(x = 3, y = -1, z = 3, w = -3),
      Pos4D(x = 1, y = -2, z = 1, w = -2),
      Pos4D(x = 3, y = -2, z = 1, w = -1),
      Pos4D(x = 2, y = -3, z = 1, w = -1),
      Pos4D(x = 1, y = -2, z = 1, w = -1),
      Pos4D(x = 3, y = -3, z = 3, w = -2),
      Pos4D(x = 3, y = -3, z = 1, w = -1),
      Pos4D(x = 1, y = -3, z = 2, w = -3),
      Pos4D(x = 2, y = -3, z = 1, w = -2),
      Pos4D(x = 3, y = -2, z = 2, w = -3),
      Pos4D(x = 1, y = -3, z = 2, w = -2),
      Pos4D(x = 1, y = -1, z = 2, w = -2),
      Pos4D(x = 3, y = -2, z = 3, w = -1),
      Pos4D(x = 1, y = -1, z = 1, w = -1),
      Pos4D(x = 1, y = -2, z = 2, w = -1),
      Pos4D(x = 3, y = -3, z = 2, w = -3),
      Pos4D(x = 3, y = -1, z = 1, w = -2),
      Pos4D(x = 1, y = -3, z = 1, w = -1),
      Pos4D(x = 2, y = -1, z = 3, w = -1),
      Pos4D(x = 3, y = -1, z = 1, w = -1),
      Pos4D(x = 2, y = -1, z = 1, w = -1),
      Pos4D(x = 1, y = -1, z = 3, w = -2),
      Pos4D(x = 2, y = -3, z = 3, w = -2),
      Pos4D(x = 2, y = -2, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 2, w = -1),
      Pos4D(x = 3, y = -3, z = 1, w = -3),
      Pos4D(x = 1, y = -1, z = 3, w = -1),
      Pos4D(x = 2, y = -1, z = 3, w = -2),
      Pos4D(x = 2, y = -1, z = 2, w = -3),
      Pos4D(x = 3, y = -3, z = 3, w = -1),
      Pos4D(x = 3, y = -1, z = 1, w = -3),
      Pos4D(x = 1, y = -3, z = 1, w = -2),
      Pos4D(x = 1, y = -3, z = 1, w = -3),
      Pos4D(x = 1, y = -2, z = 2, w = -3),
      Pos4D(x = 1, y = -3, z = 3, w = -3),
      Pos4D(x = 1, y = -2, z = 3, w = -3),
      Pos4D(x = 1, y = -3, z = 3, w = -1),
      Pos4D(x = 3, y = -1, z = 2, w = -2),
      Pos4D(x = 2, y = -3, z = 3, w = -3),
      Pos4D(x = 2, y = -1, z = 3, w = -3),
      Pos4D(x = 3, y = -3, z = 2, w = -2),
      Pos4D(x = 2, y = -2, z = 1, w = -3),
      Pos4D(x = 3, y = -2, z = 3, w = -2),
      Pos4D(x = 1, y = -3, z = 3, w = -2),
      Pos4D(x = 1, y = -1, z = 3, w = -3),
      Pos4D(x = 3, y = -2, z = 1, w = -3),
      Pos4D(x = 3, y = -2, z = 2, w = -1),
      Pos4D(x = 2, y = -1, z = 2, w = -1),
      Pos4D(x = 3, y = -1, z = 2, w = -3),
      Pos4D(x = 2, y = -3, z = 1, w = -3),
      Pos4D(x = 1, y = -2, z = 3, w = -2),
      Pos4D(x = 1, y = -1, z = 2, w = -3),
      Pos4D(x = 1, y = -2, z = 1, w = -3),
      Pos4D(x = 1, y = -3, z = 2, w = -1),
      Pos4D(x = 2, y = -1, z = 1, w = -2),
      Pos4D(x = 3, y = -1, z = 3, w = -2),
      Pos4D(x = 2, y = -2, z = 1, w = -1),
      Pos4D(x = 1, y = -1, z = 1, w = -3),
      Pos4D(x = 1, y = -1, z = 2, w = -1),
      Pos4D(x = 2, y = -3, z = 2, w = -3),
      Pos4D(x = 3, y = -2, z = 3, w = -3)
    )

    val axisOffsetsFn = Set(
      Pos4D(x = 2, y = -1, z = 2, w = -2),
      Pos4D(x = 2, y = -2, z = 2, w = -1),
      Pos4D(x = 2, y = -2, z = 2, w = -3),
      Pos4D(x = 3, y = -2, z = 2, w = -2),
      Pos4D(x = 2, y = -3, z = 2, w = -2),
      Pos4D(x = 2, y = -2, z = 1, w = -2),
      Pos4D(x = 2, y = -2, z = 3, w = -2)
    )

    val diagonalOffsetsFn = Set(
      Pos4D(x = 2, y = -1, z = 1, w = -3),
      Pos4D(x = 3, y = -1, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 3, w = -3),
      Pos4D(x = 2, y = -2, z = 3, w = -3),
      Pos4D(x = 3, y = -2, z = 1, w = -2),
      Pos4D(x = 2, y = -3, z = 2, w = -1),
      Pos4D(x = 2, y = -3, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 1, w = -2),
      Pos4D(x = 3, y = -1, z = 2, w = -1),
      Pos4D(x = 3, y = -1, z = 3, w = -3),
      Pos4D(x = 3, y = -2, z = 1, w = -1),
      Pos4D(x = 2, y = -3, z = 1, w = -1),
      Pos4D(x = 3, y = -3, z = 3, w = -2),
      Pos4D(x = 3, y = -3, z = 1, w = -1),
      Pos4D(x = 2, y = -3, z = 1, w = -2),
      Pos4D(x = 3, y = -2, z = 2, w = -3),
      Pos4D(x = 3, y = -2, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 2, w = -3),
      Pos4D(x = 3, y = -1, z = 1, w = -2),
      Pos4D(x = 2, y = -1, z = 3, w = -1),
      Pos4D(x = 3, y = -1, z = 1, w = -1),
      Pos4D(x = 2, y = -1, z = 1, w = -1),
      Pos4D(x = 2, y = -3, z = 3, w = -2),
      Pos4D(x = 2, y = -2, z = 3, w = -1),
      Pos4D(x = 3, y = -3, z = 2, w = -1),
      Pos4D(x = 3, y = -3, z = 1, w = -3),
      Pos4D(x = 2, y = -1, z = 3, w = -2),
      Pos4D(x = 2, y = -1, z = 2, w = -3),
      Pos4D(x = 3, y = -3, z = 3, w = -1),
      Pos4D(x = 3, y = -1, z = 1, w = -3),
      Pos4D(x = 3, y = -1, z = 2, w = -2),
      Pos4D(x = 2, y = -3, z = 3, w = -3),
      Pos4D(x = 2, y = -1, z = 3, w = -3),
      Pos4D(x = 3, y = -3, z = 2, w = -2),
      Pos4D(x = 2, y = -2, z = 1, w = -3),
      Pos4D(x = 3, y = -2, z = 3, w = -2),
      Pos4D(x = 3, y = -2, z = 1, w = -3),
      Pos4D(x = 3, y = -2, z = 2, w = -1),
      Pos4D(x = 2, y = -1, z = 2, w = -1),
      Pos4D(x = 3, y = -1, z = 2, w = -3),
      Pos4D(x = 2, y = -3, z = 1, w = -3),
      Pos4D(x = 2, y = -1, z = 1, w = -2),
      Pos4D(x = 3, y = -1, z = 3, w = -2),
      Pos4D(x = 2, y = -2, z = 1, w = -1),
      Pos4D(x = 2, y = -3, z = 2, w = -3),
      Pos4D(x = 3, y = -2, z = 3, w = -3)
    )

    assertResult(axisOffsets)(pos2.axisOffsets)
    assertResult(diagonalOffsets)(pos2.diagonalOffsets)
    assertResult(axisOffsets ++ diagonalOffsets)(pos2.allOffsets)

    assertResult(axisOffsetsFn)(pos2.axisOffsetsFn(p => p.x > 1))
    assertResult(diagonalOffsetsFn)(pos2.diagonalOffsetsFn(p => p.x > 1))
    assertResult(axisOffsetsFn ++ diagonalOffsetsFn)(pos2.allOffsetsFn(
      p =>
        p.x > 1
    ))

    assertResult(Pos4D(0, 0, 0, 0))(Pos4D.zero)
