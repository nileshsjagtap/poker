sealed abstract class Rank {
  def get = this.toString
  def abbreviation: String
}

case object `2` extends Rank {
  override def abbreviation = "2"
}


`2`.toString
`2`


def double: Double => Double = d => d * 2

def square: Double => Double = d => d * d

def dComposeS = double compose square

def dAndThenS = double andThen square

dComposeS(3)

dAndThenS(3)

//f(g)
//g(f)