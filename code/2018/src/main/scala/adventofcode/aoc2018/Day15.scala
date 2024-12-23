package adventofcode
package aoc2018

import utilities.AdventOfCode.*
import utilities.Graph
import utilities.Orderings.posReadingOrder
import utilities.Pos

object Day15 extends AdventOfCode(Prod):
  val map: Vector[Pos] =
    val result =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c == 'E' | c == 'G' | c == '.'
      yield Pos(x, y)

    result.toVector

  def units(elfAttackPower: Int): Vector[CombatUnit] =
    val result =
      for
        (l, y) <- input.linesIterator.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c == 'E' | c == 'G'
      yield c match
        case 'E' => CombatUnit(Pos(x, y), UnitType.Elf, 200, elfAttackPower)
        case 'G' => CombatUnit(Pos(x, y), UnitType.Goblin, 200, 3)

    result.toVector.sortBy(_.loc)

  enum UnitType:
    case Elf
    case Goblin

  case class CombatUnit(
    loc: Pos,
    unitType: UnitType,
    hitPoints: Int,
    attackPower: Int
  ):
    def isAlive: Boolean =
      hitPoints > 0

    def findShortestPath(
      map: Vector[Pos],
      targets: Vector[Pos]
    ): Option[List[Pos]] =
      import scala.collection.immutable.SortedSet
      import utilities.GraphTraversal.breadthFirstSearchPathTo

      val bfs: Graph[Pos] =
        Graph.unit: s =>
          SortedSet.empty[Pos] ++ BattleState.adjacentSquares(s, map)

      bfs.breadthFirstSearchPathTo(loc)(targets.contains)

    def attack(that: CombatUnit): CombatUnit =
      that.copy(hitPoints = that.hitPoints - attackPower)

  case class BattleState(
    map: Vector[Pos],
    openUnits: Vector[CombatUnit],
    closedUnits: Vector[CombatUnit],
    rounds: Int
  ):
    def allUnits: Vector[CombatUnit] =
      openUnits ++ closedUnits

    def elfCount: Int =
      allUnits.count(_.unitType == UnitType.Elf)

    def targetUnits(
      activeUnit: CombatUnit,
      nonActiveUnits: Vector[CombatUnit]
    ): Vector[CombatUnit] =
      nonActiveUnits.filterNot(_.unitType == activeUnit.unitType)

    def openSquaresInRangeOfTargets(
      activeUnit: CombatUnit,
      nonActiveUnits: Vector[CombatUnit]
    ): Vector[Pos] =
      targetUnits(activeUnit, nonActiveUnits)
        .flatMap(u => BattleState.adjacentSquares(u.loc, map))
        .filterNot(p => nonActiveUnits.exists(_.loc == p))

    def isFinished: Boolean =
      allUnits.distinctBy(_.unitType).length <= 1

    def outcome: Int =
      rounds * allUnits.map(_.hitPoints).sum

    def findUnit(pos: Pos): Option[CombatUnit] =
      allUnits.find(_.loc == pos)

    def updateUnit(pos: Pos, unit: CombatUnit): BattleState =
      val open   = openUnits.indexWhere(_.loc == pos)
      val closed = closedUnits.indexWhere(_.loc == pos)

      if open >= 0 then
        copy(openUnits = openUnits.updated(open, unit))
      else
        copy(closedUnits = closedUnits.updated(closed, unit))

    def canAttack(
      activeUnit: CombatUnit,
      nonActiveUnits: Vector[CombatUnit]
    ): Boolean =
      openSquaresInRangeOfTargets(activeUnit, nonActiveUnits).contains(
        activeUnit.loc
      )

    def doAttack(
      activeUnit: CombatUnit,
      nonActiveUnits: Vector[CombatUnit]
    ): BattleState =
      val unitToAttack =
        BattleState.adjacentSquares(
          activeUnit.loc,
          targetUnits(activeUnit, nonActiveUnits).map(_.loc)
        )
          .map(u => findUnit(u).get)
          .sortBy(_.loc)
          .minBy(_.hitPoints)

      val attackedUnit = activeUnit.attack(unitToAttack)

      val newBattleState =
        copy(
          openUnits = openUnits.tail,
          closedUnits = activeUnit +: closedUnits
        )
          .updateUnit(attackedUnit.loc, attackedUnit)

      newBattleState.copy(
        openUnits = newBattleState.openUnits.filter(_.isAlive),
        closedUnits = newBattleState.closedUnits.filter(_.isAlive)
      )

    def nextRound: BattleState =
      if isFinished && openUnits.nonEmpty then
        this
      else
        takeTurn.nextRound

    def nextRoundSaveElves(
      initialElfCount: Int,
      elfAttackPower: Int
    ): BattleState =
      if elfCount != initialElfCount then
        val newBattleState =
          BattleState.fromInput(map, units(elfAttackPower + 1))

        newBattleState.nextRoundSaveElves(initialElfCount, elfAttackPower + 1)
      else if isFinished && openUnits.nonEmpty then
        this
      else
        takeTurn.nextRoundSaveElves(initialElfCount, elfAttackPower)

    def takeTurn: BattleState =
      if openUnits.isEmpty then
        // printer(closedUnits, map, rounds, size = 32)
        copy(
          openUnits = closedUnits.sortBy(_.loc),
          closedUnits = Vector.empty[CombatUnit],
          rounds = rounds + 1
        )
      else
        val activeUnit     = openUnits.head
        val nonActiveUnits = openUnits.tail ++ closedUnits

        if canAttack(activeUnit, nonActiveUnits) then
          doAttack(activeUnit, nonActiveUnits)
        else
          activeUnit.findShortestPath(
            map.filterNot(p => nonActiveUnits.exists(_.loc == p)),
            openSquaresInRangeOfTargets(activeUnit, nonActiveUnits)
          ) match
            case Some(l) =>
              val movedUnit = activeUnit.copy(loc = l(1))

              if canAttack(movedUnit, nonActiveUnits) then
                doAttack(movedUnit, nonActiveUnits)
              else
                copy(
                  openUnits = openUnits.tail,
                  closedUnits = movedUnit +: closedUnits
                )
            case None =>
              copy(
                openUnits = openUnits.tail,
                closedUnits = activeUnit +: closedUnits
              )

  object BattleState:
    def fromInput(map: Vector[Pos], units: Vector[CombatUnit]): BattleState =
      BattleState(map, units, Vector.empty[CombatUnit], 0)

    def adjacentSquares(pos: Pos, map: Vector[Pos]): Vector[Pos] =
      Vector(
        pos + Pos(1, 0),
        pos + Pos(-1, 0),
        pos + Pos(0, 1),
        pos + Pos(0, -1)
      ).filter(map.contains)

  lazy val pt1: Int =
    val result =
      BattleState
        .fromInput(map, units(3))
        .nextRound

    result.outcome

  lazy val pt2: Int =
    val initialBattleState =
      BattleState.fromInput(map, units(4))

    val result =
      initialBattleState
        .nextRoundSaveElves(initialBattleState.elfCount, 4)

    result.outcome

  answer(1)(pt1)

  answer(2)(pt2)

  def printer(
    units: Vector[CombatUnit],
    map: Vector[Pos],
    rounds: Int,
    size: Int
  ): Unit =
    val box1 = units.foldLeft(Vector.fill(size, size)('#')): (acc, u) =>
      acc.updated(
        u.loc.y,
        acc(u.loc.y).updated(u.loc.x, u.unitType.toString.charAt(0))
      )
    val box2 = map.filterNot(p => units.exists(_.loc == p)).foldLeft(box1):
      (acc, loc) =>
        acc.updated(loc.y, acc(loc.y).updated(loc.x, '.'))

    print("\u001b[2J")
    println:
      "\nRound: " + rounds +
        ((0 until size).map(_.toString.last).toVector +: box2)
          .zipWithIndex
          .map: (row, y) =>
            ((y - 1 max 0).toString.last +: row)
              .map(_.toString)
              .mkString("\n", " ", "")
          .mkString
