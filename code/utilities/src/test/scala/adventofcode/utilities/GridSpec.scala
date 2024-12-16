package adventofcode
package utilities

import org.scalatest.funsuite.AnyFunSuite

class GridSpec extends AnyFunSuite:
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

    val grid6: Vector[(Int, Int, Int)] =
      Vector(
        (0, 0, 1),
        (1, 0, 2),
        (2, 0, 3),
        (0, 1, 4),
        (1, 1, 5),
        (2, 1, 6),
        (0, 2, 7),
        (1, 2, 8),
        (2, 2, 9)
      )

    val grid7: Grid[Int] =
      Grid.unit(Vector(Vector(9, 2, 3), Vector(4, 5, 6), Vector(7, 8, 1)))

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

    assertResult(grid3)(grid1.flatMap(i => Vector(i + 1)))

    assertResult(grid3)(grid1.mapWithIndex((i, _, _) => i + 1))

    assertResult(grid4)(grid1.flatten)

    assertResult(4)(grid1.count(_ % 2 == 0))

    assertResult(true)(grid2.exists(_ > 9))

    assertResult(Some(5))(grid1.lift(1, 1))

    assertResult(None)(grid1.lift(3, 0))

    assertResult(true)(grid2.contains(10))

    assertResult((3, 3))(grid1.size)

    assertResult(grid4)(grid1.iterate.toVector)

    assertResult(grid6)(grid1.iterateWithIndex.toVector)

    assertResult(Some(5))(grid1.find(_ == 5))

    assertResult(grid7)(grid1.swap(0, 0)(2, 2))

    assertResult(Some(1, 1, 5))(grid1.findWithIndex((_, _, i) => i == 5))

    assertResult(grid5)(grid1.transpose)

    assertResult(grid1)(
      grid1.rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise
    )

    assertResult(Pos(1, 1))(grid1.findPos(5))

    assertResult(grid1AsString)(grid1.asString)
