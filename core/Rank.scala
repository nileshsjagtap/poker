package scala.com.example.poker.core

sealed abstract class Rank {
  def get = this.toString
  def abbreviation: String
}

case object Ace extends Rank {
  override def abbreviation = "A"
}
case object Two extends Rank {
  override def abbreviation = "2"
}
case object Three extends Rank {
  override def abbreviation = "3"
}
case object Four extends Rank {
  override def abbreviation = "4"
}
case object Five extends Rank {
  override def abbreviation = "5"
}
case object Six extends Rank {
  override def abbreviation = "6"
}
case object Seven extends Rank {
  override def abbreviation = "7"
}
case object Eight extends Rank {
  override def abbreviation = "8"
}
case object Nine extends Rank {
  override def abbreviation = "9"
}
case object Ten extends Rank {
  override def abbreviation = "10"
}
case object Jack extends Rank {
  override def abbreviation = "J"
}
case object Queen extends Rank {
  override def abbreviation = "Q"
}
case object King extends Rank {
  override def abbreviation = "K"
}

object Rank {

  def sortRanks(ranks: List[Rank]) = ranks.sortWith(_.abbreviation < _.abbreviation).dropWhile(_ == Ace)

  def allRanks = rankMap.values.toList

  private def rankMap = Map(
    Ace.abbreviation.toLowerCase -> Ace,
    Two.abbreviation.toLowerCase -> Two,
    Three.abbreviation.toLowerCase -> Three,
    Four.abbreviation.toLowerCase -> Four,
    Five.abbreviation.toLowerCase -> Five,
    Six.abbreviation.toLowerCase -> Six,
    Seven.abbreviation.toLowerCase -> Seven,
    Eight.abbreviation.toLowerCase -> Eight,
    Nine.abbreviation.toLowerCase -> Nine,
    Ten.abbreviation.toLowerCase -> Ten,
    Jack.abbreviation.toLowerCase -> Jack,
    Queen.abbreviation.toLowerCase -> Queen,
    King.abbreviation.toLowerCase -> King
  )

  def apply(rank: String) = {
    if (validRank(rank)) Right(rankMap(rank.toLowerCase))
    else Left(InvalidRankError)
  }

  private def validRank(rank: String) = rank.nonEmpty && rankMap.keySet.map(_.toLowerCase).contains(rank.toLowerCase)

  def pointValueOf(rank: String) =
    if (rank.equals("J")) 11
    else if (rank.equals("Q")) 12
    else if (rank.equals("K")) 13
    else if (rank.equals("A")) 14
    else rank.toInt

}