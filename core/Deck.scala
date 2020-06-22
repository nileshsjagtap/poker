package scala.com.example.poker.core

class Deck(val cards: List[Card])

object Deck {

  def apply(cards: List[Card]) =
    validCards(cards).map(new Deck(_))

  private def validCards(cards: List[Card]) =
    if (cards.isEmpty) Left(InvalidDeckError)
    else if (cards.length != 52) Left(InvalidDeckError)
    else if (!ranksForAllSuitsPresent(cards)) Left(InvalidDeckError)
    else Right(cards)

  private def ranksForAllSuitsPresent(cards: List[Card]) =
    Suit.allSuits.count(suit => allRanksPresentForSuit(cards.filter(_.suit == suit), suit)) == 4

  private def allRanksPresentForSuit(cards: List[Card], suit: Suit) =
    thirteenCardsPerSuit(cards, suit) && allRanksPresent(cards)

  private def allRanksPresent(cards: List[Card]) =
    Rank.allRanks.map(rank => cards.count(card => card.rank == rank)).count(_ == 1) == 13

  private def thirteenCardsPerSuit(cards: List[Card], suit: Suit) =
    cards.count(_.suit == suit) == 13

}