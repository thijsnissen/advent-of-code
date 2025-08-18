package adventofcode
package aoc2021

import utilities.AdventOfCode.*

object Day06 extends AdventOfCode(Prod):
  val initialState: State =
    State.fromString:
      input
        .linesIterator
        .next()

  type State = Map[Int, Long]

  object State:
    lazy val empty: State =
      Map.empty[Int, Long].withDefaultValue(0)

    def fromString(s: String): State =
      s.split(",")
        .groupBy(identity)
        .map: (day, count) =>
          day.toInt -> count.length.toLong
        .withDefaultValue(0)

    extension (self: State)
      def simulate: State =
        @tailrec def loop(days: List[Int], state: State, acc: State): State =
          days.headOption match
            case None      => acc
            case Some(day) =>
              if day == 0 then
                loop(
                  days.tail,
                  state.updated(day, 0),
                  acc
                    .updated(6, acc(6) + state(0))
                    .updated(8, acc(8) + state(0))
                )
              else
                loop(
                  days.tail,
                  state.updated(day, 0),
                  acc.updated(day - 1, acc(day - 1) + state(day))
                )

        loop(self.keys.toList, self, State.empty)

      def sumAfterDays(days: Int): Long =
        Iterator
          .iterate(self)(_.simulate)
          .drop(days)
          .next()
          .values
          .sum

  import State.*

  lazy val pt1: Long =
    initialState.sumAfterDays(80)

  lazy val pt2: Long =
    initialState.sumAfterDays(256)

  answer(1)(pt1)

  answer(2)(pt2)
