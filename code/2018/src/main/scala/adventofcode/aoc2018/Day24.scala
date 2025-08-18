package adventofcode
package aoc2018

import scala.collection.immutable.SortedSet
import utilities.AdventOfCode.*
import utilities.Utilities.sumBy

object Day24 extends AdventOfCode(Prod):
  val immuneSystem: Vector[Group] =
    input
      .linesIterator
      .dropWhile(s => !s.contains("Immune System"))
      .drop(1)
      .takeWhile(_.nonEmpty)
      .map(Group.fromString(_)(ArmyType.ImmuneSystem))
      .toVector

  val infection: Vector[Group] =
    input
      .linesIterator
      .dropWhile(s => !s.contains("Infection"))
      .drop(1)
      .map(Group.fromString(_)(ArmyType.Infection))
      .toVector

  enum ArmyType:
    case ImmuneSystem, Infection

  enum AttackType:
    case Bludgeoning, Cold, Fire, Radiation, Slashing

  object AttackType:
    def fromString(s: String): AttackType =
      s.trim match
        case "bludgeoning" => Bludgeoning
        case "cold"        => Cold
        case "fire"        => Fire
        case "radiation"   => Radiation
        case "slashing"    => Slashing
        case other         => sys.error(s"Unable to parse: '$other'")

    def fromCombinedString(s: String): (Set[AttackType], Set[AttackType]) =
      @tailrec def loop(
        todo: Vector[String],
        immune: Set[AttackType],
        weak: Set[AttackType]
      ): (Set[AttackType], Set[AttackType]) =
        todo.headOption match
          case None =>
            (weak, immune)
          case Some(s"immune to $immuneTo") =>
            loop(
              todo.tail,
              immune ++ immuneTo.split(", ").map(fromString),
              weak
            )
          case Some(s"weak to $weakTo") =>
            loop(todo.tail, immune, weak ++ weakTo.split(", ").map(fromString))
          case Some(other) =>
            sys.error(s"Unable to parse: '$other'")

      loop(
        s
          .replaceAll("[()]", "")
          .split(";")
          .map(_.trim)
          .filter(_.nonEmpty)
          .toVector,
        Set.empty[AttackType],
        Set.empty[AttackType]
      )

  case class Group(
    armyType: ArmyType,
    units: Int,
    hitPoints: Int,
    attackDamage: Int,
    attackType: AttackType,
    initiative: Int,
    weaknesses: Set[AttackType],
    immunities: Set[AttackType]
  ):
    def effectivePower: Int =
      units * attackDamage

    def selectTarget(groups: SortedSet[Group]): Option[Group] =
      groups
        .filterNot(_.armyType == armyType)
        .maxByOption(damage)
        .filter(damage(_) != 0)

    def damage(defending: Group): Int =
      if defending.immunities.contains(attackType) then 0
      else if defending.weaknesses.contains(attackType) then effectivePower * 2
      else effectivePower

    def attack(defending: Group): Option[Group] =
      val lostUnits =
        damage(defending) / defending.hitPoints

      Option.unless(lostUnits >= defending.units):
        defending.copy(units = defending.units - lostUnits)

    def boost(amount: Int): Group =
      copy(attackDamage = attackDamage + amount)

  object Group:
    def fromString(s: String)(armyType: ArmyType): Group =
      s match
        case s"$units units each with $hitPoints hit points${vulnerabilities}with an attack that does $attackDamage $attackType damage at initiative $initiative" =>
          val (weaknesses, immunities) =
            AttackType.fromCombinedString(vulnerabilities.trim)

          Group(
            armyType = armyType,
            units = units.toInt,
            hitPoints = hitPoints.toInt,
            attackDamage = attackDamage.toInt,
            attackType = AttackType.fromString(attackType),
            initiative = initiative.toInt,
            weaknesses = weaknesses,
            immunities = immunities
          )

    given targetSelectionOrder: Ordering[Group] =
      Ordering.by(g => (-g.effectivePower, -g.initiative))

    given attackingOrder: Ordering[Group] =
      Ordering.by(g => -g.initiative)

  object Fight:
    def targetSelection(groups: Vector[Group]): Map[Group, Option[Group]] =
      @tailrec def loop(
        attacking: Vector[Group],
        defending: SortedSet[Group],
        acc: Map[Group, Option[Group]]
      ): Map[Group, Option[Group]] =
        attacking.headOption match
          case None        => acc
          case Some(group) =>
            val target =
              group.selectTarget(defending)

            loop(
              attacking.tail,
              target.fold(defending)(defending - _),
              acc + (group -> target)
            )

      import Group.targetSelectionOrder

      loop(
        groups.sorted,
        SortedSet.empty[Group] ++ groups,
        Map.empty[Group, Option[Group]]
      )

    def attacking(groups: Map[Group, Option[Group]]): Vector[Group] =
      @tailrec def loop(
        todo: Vector[Group],
        targets: Map[Group, Option[Group]]
      ): Vector[Group] =
        todo.headOption match
          case None            => targets.keys.toVector
          case Some(attacking) =>
            targets(attacking) match
              case Some(defending) =>
                attacking.attack(defending) match
                  case None => loop(
                      todo.tail.filterNot(_ == defending),
                      targets - defending
                    )
                  case Some(defended) => loop(
                      todo.tail.map: g =>
                        if g == defending then defended else g,
                      targets - defending + (defended -> targets(defending))
                    )
              case None => loop(todo.tail, targets)

      import Group.attackingOrder

      loop(groups.keys.toVector.sorted, groups)

    def fight(groups: Vector[Group]): Vector[Group] =
      targetSelection
        .andThen(attacking)(groups)

  lazy val pt1: Int =
    Iterator
      .iterate(immuneSystem ++ infection)(Fight.fight)
      .dropWhile(_.distinctBy(_.armyType).length == 2)
      .next
      .sumBy(_.units)

  lazy val pt2: Int =
    @tailrec def loop(groups: Vector[Group], boost: Int = 0): Vector[Group] =
      val result = Fight.fight(groups)

      if result.forall(_.armyType == ArmyType.ImmuneSystem) then
        result
      else if groups == result then
        loop(immuneSystem.map(_.boost(boost)) ++ infection, boost + 1)
      else loop(result, boost)

    loop(immuneSystem ++ infection)
      .sumBy(_.units)

  answer(1)(pt1)

  answer(2)(pt2)
