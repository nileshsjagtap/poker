package com.poker.core

import scala.com.example.poker.core.{Card, InvalidPlayerError}

class Player(val name: String, val cards: List[Card])

object Player {

  def apply(name: String): Either[InvalidPlayerError.type, Player] =
    if(name.nonEmpty) Right(new Player(name, List()))
    else Left(InvalidPlayerError)

  def apply(name: String, cards: List[Card]): Either[InvalidPlayerError.type, Player] =
    if(name.nonEmpty && cards.nonEmpty) Right(new Player(name, cards))
    else Left(InvalidPlayerError)

  def assignCards(player: Player, cards: List[Card]) = new Player(player.name, cards)

}

case class PlayerPokerHand(player: Player, pokerHand: PokerHand)
case class PlayerPokerHands(list: List[PlayerPokerHand]) {

  def winner = {
    val sorted = list.zip(list.map(_.pokerHand.priority)).sortWith(_._2 < _._2)
    sorted.filter(_._2 == sorted.head._2).map(_._1)
  }

}