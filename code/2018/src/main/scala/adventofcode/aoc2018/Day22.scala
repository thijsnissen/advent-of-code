package adventofcode
package aoc2018

import utilities.AdventOfCode.*
import utilities.Box
import utilities.Pos

object Day22 extends AdventOfCode(Prod):
  import Tool.*

  val depth: Int =
    input
      .linesIterator
      .collect:
        case s"depth: $d" => d.toInt
      .next

  val target: Pos =
    input
      .linesIterator
      .collect:
        case s"target: $x,$y" => Pos(x.toInt, y.toInt)
      .next

  lazy val mouthOfCave =
    Pos.zero

  enum Tool:
    case Torch, ClimbingGear, Neither

  enum RegionType(val tools: Set[Tool]):
    case Rocky  extends RegionType(Set(ClimbingGear, Torch))
    case Wet    extends RegionType(Set(ClimbingGear, Neither))
    case Narrow extends RegionType(Set(Torch, Neither))

  object RegionType:
    def fromErosionLevel(erosionLevel: Int): RegionType =
      fromOrdinal(erosionLevel % 3)

  case class State(pos: Pos, regionType: RegionType, tool: Tool):
    def toggleTool: State =
      copy(tool = (regionType.tools - tool).head)

  object State:
    given stateOrdering(using ord: Ordering[Pos]): Ordering[State] =
      Ordering.comparatorToOrdering: (a, b) =>
        ord.compare(a.pos, b.pos)

  case class Regions(
    mouthOfCave: Pos,
    target: Pos,
    depth: Int,
    regions: Map[Pos, RegionType],
  ):
    def riskLevel: Int =
      regions.map((_, rt) => rt.ordinal).sum

    def next(state: State): Map[State, Int] =
      val next =
        state
          .pos
          .axisOffsetsFn(
            p =>
              regions.contains(p) && regions(p).tools.contains(state.tool)
          )
          .map(p => State(p, regions(p), state.tool) -> 1)
          .toMap

      next + (state.toggleTool -> 7)

  object Regions:
    def unit(
      mouthOfCave: Pos,
      target: Pos,
      depth: Int,
      multiplier: Int,
    ): Regions =
      val init: Regions =
        Regions(mouthOfCave, target, depth, Map.empty[Pos, RegionType])

      val regions: Map[Pos, RegionType] =
        Box(mouthOfCave, target * multiplier)
          .iterator
          .foldLeft(Map.empty[Pos, Int]): (acc, p) =>
            getErosionLevels(p, init, acc)
          .map((p, el) => p -> RegionType.fromErosionLevel(el))

      init.copy(regions = regions)

    def getErosionLevels(
      pos: Pos,
      regions: Regions,
      acc: Map[Pos, Int],
    ): Map[Pos, Int] =
      def erosionLevel(geologicIndex: Int): Int =
        (geologicIndex + regions.depth) % 20183

      pos match
        case p if p == regions.mouthOfCave || p == regions.target =>
          acc + (pos -> erosionLevel(0))
        case Pos(x, 0) => acc + (pos -> erosionLevel(x * 16807))
        case Pos(0, y) => acc + (pos -> erosionLevel(y * 48271))
        case Pos(x, y) =>
          val levelsA: Map[Pos, Int] = acc.get(Pos(x - 1, y)) match
            case Some(_) => acc
            case None    => getErosionLevels(Pos(x - 1, y), regions, acc)

          val levelsB: Map[Pos, Int] = acc.get(Pos(x, y - 1)) match
            case Some(_) => acc
            case None    => getErosionLevels(Pos(x, y - 1), regions, acc)

          (levelsA ++ levelsB) + (pos -> erosionLevel(
            levelsA(Pos(x - 1, y)) * levelsB(Pos(x, y - 1))
          ))

  lazy val pt1: Int =
    Regions
      .unit(mouthOfCave, target, depth, 1)
      .riskLevel

  lazy val pt2: Int =
    import utilities.WeightedGraph
    import utilities.AStar.*

    import State.stateOrdering
    import utilities.Orderings.posReadingOrder

    val regions: Regions =
      Regions
        .unit(mouthOfCave, target, depth, 3)

    val graph: WeightedGraph[State] =
      WeightedGraph.unit: s =>
        regions.next(s)

    val init: State =
      State(mouthOfCave, regions.regions(mouthOfCave), Torch)

    val last: State =
      State(target, regions.regions(target), Torch)

    val heuristic: (State, Int) => Double =
      (s, e) => (if e == 1 then 0 else 7) + s.pos.euclidean(last.pos)

    graph.shortestPathTo(init, _ == last)(heuristic) match
      case Some(dist, _) => dist
      case None => sys.error(s"No path found from ${init.pos} to ${last.pos}")

  answer(1)(pt1)

  answer(2)(pt2)
