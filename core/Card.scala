package scala.com.example.poker.core

case class Card (suit: Suit, rank: Rank)

object Card {

  def apply(suit: String, rank: String): Either[Error, Card] = for {
    suit <- Suit(suit)
    rank <- Rank(rank)
  } yield Card(suit, rank)

}