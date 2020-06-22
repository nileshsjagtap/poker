package com.poker

object Main {

  def main(args: Array[String]): Unit = {
  import com.poker.core.PokerGame._

    val winners = for {
      players <- createPlayers(List("abu", "nanu", "nilu", "aadu", "puju"))
      deck <- createDeck()
      playersWithHands <- createHandsAndAssignToPlayers(players, deck)
      winningPlayer <- winner(playersWithHands)
    } yield winningPlayer

    if (winners.isLeft) println("""Error:[\"${winners.left.get}\"] occured.""")
    else if (winners.isRight && winners.right.get.size == 1) println(s"Winner is ${winners.right.get.head.player.name} with cards ${winners.right.get.head.pokerHand.toString}")
    else println(s"Winners are ${winners.right.get.map(_.player.name).mkString(", ")} with cards ${winners.right.get.map(_.pokerHand.toString).mkString(", ")}")

  }

}
