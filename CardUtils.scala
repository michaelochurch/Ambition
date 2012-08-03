object CardUtils {
  sealed trait CardDisplayMode
  case object ThreeCharAscii extends CardDisplayMode
  case object TwoCharAscii extends CardDisplayMode
  case object Unicode extends CardDisplayMode

  sealed trait CardColoringMode
  case object FourColor extends CardColoringMode
  case object TwoColor extends CardColoringMode
  case object OneColor extends CardColoringMode

  object Rank extends Enumeration {
    type T = Value
    val R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA = Value
    
    val TwoCharReprs = "23456789".map("%c ".format(_)).toArray ++ List("10") ++ "JQKA".map("%c ".format(_))
    val OneCharReprs = "23456789TJQKA"
  }

  object Suit extends Enumeration {
    type T = Value
    val Diamond, Spade, Heart, Club = Value
    
    val Reprs = "♦♠♥♣"
    val AsciiReprs = "dshc"
  }

  import Rank.{R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA}
  import Suit.{Diamond, Spade, Heart, Club}

  case class Card(rank:Rank.T, suit:Suit.T) {
    def isHonor():Boolean = rank >= RJ

    def repr(displayMode:CardDisplayMode):String = {
      displayMode match {
        case ThreeCharAscii => "%s%c".format(Rank.TwoCharReprs(rank.id), Suit.AsciiReprs(suit.id))
        case Unicode        => "%s%c".format(Rank.TwoCharReprs(rank.id), Suit.Reprs(suit.id))
        case TwoCharAscii   => "%c%c".format(Rank.OneCharReprs(rank.id), Suit.AsciiReprs(suit.id))
      }
    }

    def color(coloringMode:CardColoringMode):Option[Display.Color] = {
      coloringMode match {
        case FourColor => {
          suit match {
            case Club    => None  // Black on white background; white on black.
            case Diamond => Some(Display.Blue(true))
            case Heart   => Some(Display.Red(false))
            case Spade   => Some(Display.Green(false))
          }
        }
        case TwoColor => {
          suit match {
            case (Diamond | Heart) => Some(Display.Red(false))
            case _ => None
          }
        }
        case OneColor => None
      }
    }

    def twoChar() = {
      repr(TwoCharAscii)
    }
    
    override def toString = {
      repr(Unicode)
    }
  }

  def cardOfHumanInput(input:String):Card = {
    def fail() = {
      throw new IllegalArgumentException("cardOfHumanInput: " + input)
    }

    var rank:Rank.T = null
    var suit:Suit.T = null

    def setSuit(s:Suit.T) = {
      if (suit == null) suit = s
      else if (suit != s) fail()
    }

    def setRank(r:Rank.T) = {
      if (rank == null) rank = r
      else if (rank != r) fail()
    }

    for (c <- input) {
      c match {
        case ('c' | 'C' | '♣' | '♧') => setSuit(Club)
        case ('d' | 'D' | '♦' | '♢') => setSuit(Diamond)
        case ('s' | 'S' | '♠' | '♤') => setSuit(Spade)
        case ('h' | 'H' | '♥' | '♡') => setSuit(Heart)
        case ('a' | 'A') => setRank(RA)
        case ('k' | 'K') => setRank(RK)
        case ('q' | 'Q') => setRank(RQ)
        case ('j' | 'J') => setRank(RJ)
        case ('t' | 'T' | '1' | '0') => setRank(R10)
        case '9' => setRank(R9)
        case '8' => setRank(R8)
        case '7' => setRank(R7)
        case '6' => setRank(R6)
        case '5' => setRank(R5)
        case '4' => setRank(R4)
        case '3' => setRank(R3)
        case '2' => setRank(R2)
        case _ => fail()
      }
    }    
    if (rank == null || suit == null) fail() else Card(rank,suit)
  }
}
