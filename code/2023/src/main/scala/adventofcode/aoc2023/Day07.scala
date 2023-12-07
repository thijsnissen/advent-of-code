package adventofcode
package aoc2023

import utilities.AdventOfCode.*

object Day07 extends AdventOfCode(Prod):
  val hands: Vector[Hand] =
    input
      .linesIterator
      .map(Hand.fromString)
      .toVector

  enum HandType:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair,
      HighCard

  object HandType:
    def fromString(s: String): HandType =
      s.distinct.length match
        case 1                      => FiveOfAKind
        case 2 if isFourOfAKind(s)  => FourOfAKind
        case 2                      => FullHouse
        case 3 if isThreeOfAKind(s) => ThreeOfAKind
        case 3                      => TwoPair
        case 4                      => OnePair
        case 5                      => HighCard

    def isFourOfAKind(s: String): Boolean =
      s.count(_ == s.head) == 1 || s.count(_ == s.head) == 4

    def isThreeOfAKind(s: String): Boolean =
      s.groupBy(identity).values.exists(_.length == 3)

  case class Hand(cards: String, bid: Int, handType: HandType):
    lazy val mostOccurringCard: Char =
      val (card, _) =
        cards
          .filterNot(_ == 'J')
          .groupBy(identity)
          .map((c: Char, s: String) => (c, s.length))
          .maxByOption((_: Char, l: Int) => l)
          .getOrElse(('J', 0))

      card

    lazy val withJoker: Hand =
      copy(
        cards = cards.replace('J', 'X'),
        handType = HandType.fromString(cards.replace('J', mostOccurringCard))
      )

  object Hand:
    def fromString(s: String): Hand =
      s match
        case s"$cards $bid" =>
          Hand(cards, bid.toInt, HandType.fromString(cards))

    def cardToInt(card: Char): Int =
      if card.isDigit then card.asDigit
      else
        card match
          case 'A' => 14
          case 'K' => 13
          case 'Q' => 12
          case 'J' => 11
          case 'T' => 10
          case 'X' => 1

    extension (hands: Vector[Hand])
      def winnings: Int =
        hands
          .sorted
          .zipWithIndex
          .map((hand: Hand, rank: Int) => hand.bid * (rank + 1))
          .sum

    given (using int: Ordering[Int]): Ordering[Hand] with
      override def compare(a: Hand, b: Hand): Int =
        if a.handType.ordinal != b.handType.ordinal then
          int.compare(a.handType.ordinal, b.handType.ordinal) * -1
        else
          @tailrec
          def loop(cards: IndexedSeq[(Char, Char)]): Int =
            val (aCard, bCard) = cards.head

            if aCard == bCard then loop(cards.tail)
            else int.compare(cardToInt(aCard), cardToInt(bCard))

          loop(a.cards.zip(b.cards))

  lazy val pt1: Int =
    hands.winnings

  lazy val pt2: Int =
    hands.map(_.withJoker).winnings

  answer(1)(pt1)

  answer(2)(pt2)
