package com.poker.core

import scala.com.example.poker.core._
import scala.util.Random

object PokerGame {

  def createPlayers(names: List[String]) = sequence(names.map(Player(_)))

  def createPeck = {
    val suits: Seq[Suit] = Suit.allSuits
    val ranks: Seq[Rank] = Rank.allRanks
    suits.flatMap(s => ranks.map((s, _))).map(t => Card(t._1, t._2)).toList
  }

  def shuffleCards(cards: List[Card]) = Random.shuffle(cards)

  def createDeck(): Either[InvalidDeckError.type, Deck] = Deck(shuffleCards(createPeck))

  def createHandsAndAssignToPlayers(players: List[Player], deck: Deck) =
    if (invalidNumberOf(players)) Left(InvalidNumberOfPlayersError)
    else Right(createHands(deck, players.size).zip(players).map(zip => Player.assignCards(zip._2, zip._1)))

  def createHands(deck: Deck, numberOfPlayers: Int) = deck.cards.grouped(5).toList.take(numberOfPlayers)

  private def invalidNumberOf(players: List[Player]) = players.size > 10 || players.size < 2

  def winner(players: List[Player]) =
    if (players.count(_.cards.isEmpty) > 0) Left(HandsNotAssignedError)
    else pokerHandsFor(players).map(_.winner)
  
  def pokerHandsFor(players: List[Player]) =
    if (players.count(_.cards.isEmpty) > 0) Left(HandsNotAssignedError)
    else sequence(players.map(p => PokerHand(p.cards))).map(phs => players.zip(phs)).map(tup => tup.map(PlayerPokerHand.tupled)).map(PlayerPokerHands)

  private def sequence[L, R](listOfEither: List[Either[L, R]]): Either[L, List[R]] = {
    def iterate(remaining: List[Either[L, R]], buffer: Either[L, List[R]]): Either[L, List[R]] = remaining match {
      case Nil => buffer
      case head :: _ if head.isLeft => Left(head.left.get)
      case head :: tail => iterate(tail, Right(buffer.right.get :+ head.right.get))
    }
    iterate(listOfEither, Right(List[R]()))
  }

}