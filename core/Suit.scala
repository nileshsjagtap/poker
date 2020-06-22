package scala.com.example.poker.core

sealed abstract class Suit {
  def get = this.toString
  def abbreviation = this.toString.head.toString
}

case object Hearts extends Suit
case object Spade extends Suit
case object Diamond extends Suit
case object Club extends Suit

object Suit {

  def allSuits = suitMap.values.toList

  private def suitMap = Map(
    Hearts.get.toLowerCase -> Hearts,
    Spade.get.toLowerCase -> Spade,
    Diamond.get.toLowerCase -> Diamond,
    Club.get.toLowerCase -> Club
  )

  def apply(suit: String): Either[Error, Suit] = {
    if (validSuit(suit)) Right(suitMap(suit.toLowerCase))
    else Left(InvalidSuitError)
  }

  private def validSuit(suit: String) = suit.nonEmpty && suitMap.keySet.map(_.toLowerCase).contains(suit.toLowerCase)

}