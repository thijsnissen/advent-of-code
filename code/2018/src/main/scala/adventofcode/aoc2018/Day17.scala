package adventofcode
package aoc2018

import utilities.AdventOfCode.*
import utilities.Box
import utilities.Pos

object Day17 extends AdventOfCode(Prod):
  import Tiles.*
  import scala.collection.immutable.Queue

  val clay: Tiles =
    Tiles.fromMap:
      input
        .linesIterator
        .collect(l => Tiles.fromString(l))
        .flatten
        .map((_, TileType.Clay))
        .toMap

  lazy val springOfWater: Pos =
    Pos(500, 0)

  opaque type Tiles = Map[Pos, TileType]

  object Tiles:
    enum TileType:
      case Flow
      case Still
      case Clay
      case Sand

    def unit: Tiles =
      Map
        .empty[Pos, TileType]
        .withDefaultValue(TileType.Sand)

    def fromMap(m: Map[Pos, TileType]): Tiles =
      m.withDefaultValue(TileType.Sand)

    def fromString(s: String): Iterator[Pos] =
      s match
        case s"x=$x, y=$y1..$y2" =>
          Box(Pos(x.toInt, y1.toInt), Pos(x.toInt, y2.toInt)).iterator
        case s"y=$y, x=$x1..$x2" =>
          Box(Pos(x1.toInt, y.toInt), Pos(x2.toInt, y.toInt)).iterator

    @annotation.tailrec
    def moveWater(queue: Queue[Pos], tiles: Tiles, maxY: Int): Tiles =
      def tryDown(pos: Pos): Option[Pos] =
        if pos.y > maxY then
          None
        else
          Some(pos + Pos(0, 1))

      @annotation.tailrec
      def trySide(
        pos: Pos,
        vector: Pos,
        modified: Vector[Pos],
      ): (Option[Pos], Vector[Pos]) =
        tiles(pos + Pos(0, 1)) match
          case TileType.Sand | TileType.Flow =>
            (Some(pos), modified)
          case TileType.Clay | TileType.Still =>
            tiles(pos) match
              case TileType.Sand | TileType.Flow =>
                trySide(pos + vector, vector, pos +: modified)
              case TileType.Clay | TileType.Still =>
                (None, modified)

      queue.distinct.dequeueOption match
        case None => tiles
        case Some(pos, tail) =>
          tiles(pos) match
            case TileType.Sand | TileType.Flow =>
              tryDown(pos) match
                case Some(p) => moveWater(
                    tail.enqueue(p),
                    tiles + (pos -> TileType.Flow),
                    maxY,
                  )
                case None => moveWater(tail, tiles, maxY)
            case TileType.Clay | TileType.Still =>
              val (leftPos, leftTiles) =
                trySide(pos - Pos(0, 1), Pos(-1, 0), Vector.empty[Pos])
              val (rightPos, rightTiles) =
                trySide(pos - Pos(0, 1), Pos(1, 0), Vector.empty[Pos])

              val newTiles = leftTiles ++ rightTiles

              (leftPos, rightPos) match
                case (Some(lp), Some(rp)) =>
                  moveWater(
                    tail.enqueueAll(List(lp, rp)),
                    tiles ++ newTiles.map(_ -> TileType.Flow),
                    maxY,
                  )
                case (Some(lp), None) =>
                  moveWater(
                    tail.enqueue(lp),
                    tiles ++ newTiles.map(_ -> TileType.Flow),
                    maxY,
                  )
                case (None, Some(rp)) =>
                  moveWater(
                    tail.enqueue(rp),
                    tiles ++ newTiles.map(_ -> TileType.Flow),
                    maxY,
                  )
                case (None, None) =>
                  moveWater(
                    tail.enqueue(pos - Pos(0, 2)),
                    tiles ++ newTiles.map(_ -> TileType.Still),
                    maxY,
                  )

  lazy val pt1: Int =
    val Box(min, max) =
      Box.bounding(clay.keys)

    val boundingBox =
      Box(min - Pos(1, 0), max + Pos(1, 0))

    val result =
      moveWater(Queue(springOfWater), clay, max.y)

    // printer(boundingBox.min, boundingBox.max, result)

    result
      .filter((p, _) => boundingBox.contains(p))
      .count((_, t) => t == TileType.Still || t == TileType.Flow)

  lazy val pt2: Int =
    val Box(_, max) =
      Box.bounding(clay.keys)

    moveWater(Queue(springOfWater), clay, max.y)
      .count((_, t) => t == TileType.Still)

  answer(1)(pt1)

  answer(2)(pt2)

  def printer(min: Pos, max: Pos, result: Tiles): Unit =
    val print =
      for
        y <- min.y to max.y
        x <- min.x to max.x
      yield result(Pos(x, y)) match
        case TileType.Still => '~'
        case TileType.Flow  => '|'
        case TileType.Clay  => '#'
        case TileType.Sand  => '.'

    val delta = max.x - min.x + 1

    val print2 =
      print
        .zipWithIndex
        .map(
          (c, i) =>
            if (i + 1) % delta == 0 then c.toString + "\n" else c.toString
        )

    println(print2.mkString("\n", "", "\n"))
