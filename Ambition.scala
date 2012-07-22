import scala.util.Random

sealed trait DisplayMode
case object PrettyPrint extends DisplayMode    // for human users.
case object ThreeCharAscii extends DisplayMode // human users whose consoles don't support Unicode.
case object TwoCharAscii extends DisplayMode   // for machines / communication. 'TD' instead of '10D'.

object Config {
  // TODO: this should be a case object, not a string.
  var displayMode = PrettyPrint
}

object Rank extends Enumeration {
  type T = Value
  val R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA = Value

  val TwoCharReprs = "23456789".map(" %c".format(_)).toArray ++ List("10") ++ "JQKA".map(" %c".format(_))
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

case class RankOps(r:Rank.T) {
  def isHonor():Boolean = (r >= RJ)
}

object RankOps {
  implicit def rankPimp(r:Rank.T) = RankOps(r)
}

import RankOps.{rankPimp}

case class Card(rank:Rank.T, suit:Suit.T) {
  def isHonor():Boolean = rank.isHonor

  def repr(displayMode:DisplayMode):String = {
    displayMode match {
      case ThreeCharAscii => "%s%c".format(Rank.TwoCharReprs(rank.id), Suit.AsciiReprs(suit.id))
      case PrettyPrint    => "%s%c".format(Rank.TwoCharReprs(rank.id), Suit.Reprs(suit.id))
      case TwoCharAscii   => "%c%c".format(Rank.OneCharReprs(rank.id), Suit.AsciiReprs(suit.id))
    }
  }

  override def toString = repr(Config.displayMode)
}

class StupidHuman(msg:String) extends Exception(msg)

object StupidHuman {
  def apply(msg:String):Nothing = throw new StupidHuman(msg)
}

object CardUtils {
  def cardOfHumanInput(input:String):Card = {
    var rank:Rank.T = null
    var suit:Suit.T = null

    def setSuit(s:Suit.T) = {
      if (suit == null) suit = s
      else if (suit != s) StupidHuman(input)
    }

    def setRank(r:Rank.T) = {
      if (rank == null) rank = r
      else if (rank != r) StupidHuman(input)
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
        case _ => StupidHuman(input)
      }
    }    
    if (rank == null || suit == null) StupidHuman(input)

    Card(rank,suit)
  }
}

object PointValue {
  private[this] def points(r:Rank.T, s:Suit.T):Int = {
    s match {
      case Club    => if (r == RK)            17 else 0
      case Diamond => if (r == R8)             8 else if (r.isHonor) 3 else 1
      case Heart   => if (r == R2)             8 else if (r.isHonor) 3 else 1
      case Spade   => if (r == R8 || r == RA)  8 else if (r.isHonor) 5 else 2
    }
  }

  private[this] val pointValues = 
    Array.tabulate(4, 13) { (s, r) => points(Rank(r), Suit(s)) }

  def apply(rank:Rank.T, suit:Suit.T):Int = pointValues(suit.id)(rank.id)
  
  def apply(card:Card):Int = apply(card.rank, card.suit)

  // Apologies for this ugly code.
  def display() = {
    val bar          = "--------------------------"
    val suitStr      = "|    | ♦  | ♠  | ♥  | ♣  |"
    val suitStrAscii = "|    | D  | S  | H  | C  |"
    val crossbar     = "+----+----+----+----+----+"
    def rankStr(rankId:Int) = "| %-2s | %2d | %2d | %2d | %2d |".format(
      Rank.TwoCharReprs(rankId) +: (0 to 3).map(pointValues(_)(rankId)):_*)
    println(bar)
    if (Config.displayMode == PrettyPrint) println(suitStr) else println(suitStrAscii)
    println(crossbar)
    for (rid <- 12 to 9 by -1) println(rankStr(rid))
    println(crossbar)
    for (rid <- 8 to 0 by -1) println(rankStr(rid))
    println(bar)
  }
}

object Deck {
  val full = for (s <- Suit.values.toList; r <- Rank.values.toList) yield Card(r, s)

  def shuffled(rng:Random):Seq[Card] = rng.shuffle(full)

  def deal(rng:Random):Vector[Seq[Card]] = {
    val s = shuffled(rng)
    Vector(s.slice(0, 13), s.slice(13, 26), s.slice(26, 39), s.slice(39, 52))
  }
}

object TrickLogic {
  // This could be optimized.
  def winner(cards:Array[Card], leadPos:Int):Int = {
    assert(0 <= leadPos && leadPos < 4 && cards.length == 4)
    val ledSuit = cards(leadPos).suit
    val honor = cards.exists(c => c.suit == ledSuit && c.isHonor)
    val winningCard = cards maxBy {c => 
      if (c.suit != ledSuit) -100
      else if (honor && c.rank == R2) 100
      else c.rank.id
    }
    val res = cards.indexOf(winningCard)
    assert(0 <= res && res < 4)
    res
  }

  // Optimization: trickValue and winner can be coupled if speed becomes important.
  def trickValue(cards:Array[Card]):Int = {
    cards.map(PointValue(_)).sum
  }
}

class Hand(cards:Seq[Card]) {
  private val cardSet = cards.toSet
  def toSet():Set[Card] = cardSet
  override def toString = 
    cardSet.toList.sortBy(c => (c.suit.id << 4) + (if (c.rank == R2) 15 else c.rank.id)).mkString("Hand(", " ", " )")
}

object Hand {
  implicit def handToSet(h:Hand):Set[Card] = h.toSet
}

import Hand.{handToSet}

object RoundLogic {
  val DefaultRNG = new Random()

  case class View (
    playerId : Int,
    trickNumber : Int,
    leadPos : Int,
    pointsTaken : Vector[Int],
    table : Vector[Option[Card]],
    hand : Hand) {
    
    def legalMoves():Set[Card] = {
      if (leadPos == playerId) {
        if (trickNumber == 1) {
          // Initial lead is 8D.
          assert(hand.contains(Card(R8, Diamond)))
          Set(Card(R8,Diamond))
        } else {  // Player in lead can play anything.
          hand.toSet
        }
      } else {
        val ledSuit = table(leadPos).get.suit
        hand.filter(c => c.suit == ledSuit)
      }
    }
  }

  object DisplayView {}
//     def display(playerId:Int) = {
//       def spaces(n:Int) = new String(Array.fill(n)(' '))

//       def cardRepr(cardOpt:Card, wasLed:Boolean, canWin:Boolean) = {
//         val fmtString = (wasLed, canWin) match {
//           case (true, true) => ".(%3d)."
//           case (true, false) => " .%3d. "
//           case (false, true) => " (%3d) "
//           case (false, false) => "  %3d  "
//         }
//         fmtString.format(card match {
//           case Some(card) => card.toString
//           case None => ".?."
//         })
//       }

//       val trickNumberStr = "Trick %d of 13\n".format(v.trickNumber)
//       val youAreStr      = "              " + spaces(8 * playerId) + " ↓You↓ "
//       val pointsTakenStr = "Points taken:   %3d    %3d    %3d    %3d".format(pointsTaken:_*)
//       // TODO: fix this. 
//       val onTableStr     = "on table    : %7s %7s %7s %7s".format(table.map(cardRepr(_, false, false)))
//       val yourHandStr    = "Your hand: " + v.hand.toString
//       val legalMovesStr  = "Legal moves are: " legalMoves.mkString(", ")
//       println(trickNumberStr)
//       println(youAreStr)
//       println(pointsTakenStr)
//       println(onTableStr)
//       println(yourHandStr)
//       println(legamoMovesStr)
//     }
//   }
  
  case class State (
    rng : Random,
    trickNumber : Int,
    leadPos : Int,
    pointsTaken : Vector[Int],
    table : Vector[Option[Card]],
    hands : Vector[Hand]) {

    def view(playerId:Int):View = {
      View(playerId = playerId, trickNumber = trickNumber, leadPos = leadPos, pointsTaken = pointsTaken, 
           table = table, hand = hands(playerId))
    }
  }

  def newRound(implicit rng:Random = DefaultRNG):State = {
    val hands = Deck.deal(rng).map(new Hand(_)) 
    State(rng = rng,
          trickNumber = 0, leadPos = -1, pointsTaken = Vector.fill(4)(0),
          table = Vector.fill(4)(None), hands = hands)
  }

  trait TrickStrategy extends Function1[View, Card]

  class RandomLegalMove(private val rng:Random) extends TrickStrategy {
    def apply(v:View):Card = {
      val legalMoves = v.legalMoves.toArray
      val idx = (rng.nextDouble() * legalMoves.length).toInt
      legalMoves(idx)
    }
  }

  class FromConsole extends TrickStrategy {
    def apply(v:View) = {
      throw new Exception("not implemented yet")
    }
  }
}

object Ambition extends App {
  override def main(args:Array[String]) = {
    print("--> ")
    val line = Console.readLine
    println("You gave me: " + line)
  }
}
