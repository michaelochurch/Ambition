import scala.util.Random

sealed trait DisplayMode {
  def isUnicode = false
}
case object Unicode extends DisplayMode {
  override def isUnicode = true
}                                              // for human users.
case object ThreeCharAscii extends DisplayMode // human users whose consoles don't support Unicode.
case object TwoCharAscii extends DisplayMode   // for machines / communication. 'TD' instead of '10D'.

object Config {
  var displayMode = Unicode
  var noviceMode  = true
  var displayTricks = true
}

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

class RankOps(val r:Rank.T) {
  def isHonor():Boolean = (r >= RJ)
}

object RankOps {
  implicit def rankPimp(r:Rank.T) = new RankOps(r)
}

import RankOps.{rankPimp}

case class Card(rank:Rank.T, suit:Suit.T) {
  def isHonor():Boolean = rank.isHonor

  def repr(displayMode:DisplayMode):String = {
    displayMode match {
      case ThreeCharAscii => "%s%c".format(Rank.TwoCharReprs(rank.id), Suit.AsciiReprs(suit.id))
      case Unicode        => "%s%c".format(Rank.TwoCharReprs(rank.id), Suit.Reprs(suit.id))
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

  //Apologies for this ugly code.
  def display() = {
    val bar          = "--------------------------"
    val suitStr      = "|    | ♦  | ♠  | ♥  | ♣  |"
    val suitStrAscii = "|    | D  | S  | H  | C  |"
    val crossbar     = "+----+----+----+----+----+"
    def rankStr(rankId:Int) = "| %-2s | %2d | %2d | %2d | %2d |".format(
      Rank.TwoCharReprs(rankId) +: (0 to 3).map(pointValues(_)(rankId)):_*)
    println(bar)
    if (Config.displayMode.isUnicode) println(suitStr) else println(suitStrAscii)
    println(crossbar)
    for (rid <- 12 to 9 by -1) println(rankStr(rid))
    println(crossbar)
    for (rid <- 8 to 0 by -1) println(rankStr(rid))
    println(bar)
  }
}

object DisplayTable {
  def spaces(n:Int) = {
    if (n <= 0) "" else new String(Array.fill(n)(' '))
  }

  def pad(s:String, size:Int) = {
    val left  = (size + 1 - s.length) / 2
    val right = (size - s.length) / 2
    spaces(left) + s + spaces(right)
  }

  // Hideously inefficient, but I don't care because it's only going to be used
  // for presentation to human players and the tables will be small. 
  def apply(table:Seq[Seq[String]]):String = {
    val buffer = new StringBuffer
    val numberOfCols = table.maxBy(_.length).length
    val columnSizes = (0 until numberOfCols).map {i => 
      // TODO: write maxValue
      table.maxBy(row =>
        row.lift(i).map(_.length).getOrElse(0)).apply(i).length
    }
    for (row <- table) {
      (0 until numberOfCols).foreach {i =>
        buffer append (pad(row.lift(i).getOrElse(""), columnSizes(i)) + " ")
      }
      buffer append "\n"
    }
    buffer.toString
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
  // This will probably need to be optimized.
  def winner(cards:IndexedSeq[Option[Card]], leadPos:Int):Option[Int] = {
    assert(0 <= leadPos && leadPos < 4 && cards.length == 4)
    cards(leadPos) match {
      case None => None
      case Some(ledCard) => {
        val ledSuit = ledCard.suit 
        val honor = cards exists {
          case None =>    false
          case Some(c) => c.suit == ledSuit && c.isHonor
        }
        val winningCard = cards maxBy {
          case None => -100
          case Some(c) => 
            if (c.suit != ledSuit) -100
            else if (honor && c.rank == R2) 100
            else c.rank.id
        }
        Some(cards.indexOf(winningCard))
      }
    }
  }

  // Optimization: trickValue and winner can be coupled if speed becomes important.
  def trickValue(cards:IndexedSeq[Option[Card]]):Int = {
    cards map {
      case None    => 0
      case Some(c) => PointValue(c)
    } sum
  }
}

class Hand(cards:Seq[Card]) {
  private val cardSet = cards.toSet

  def toSet():Set[Card] = cardSet

  override def toString = 
    cardSet.toList.sortBy(c => (c.suit.id << 4) + (if (c.rank == R2) 15 else c.rank.id)).mkString("  ")

  def remove(card:Card) = {
    assert(cardSet.contains(card))
    new Hand((cardSet - card).toSeq)
  }
}

object Hand {
  implicit def handToSet(h:Hand):Set[Card] = h.toSet
}

import Hand.{handToSet}

// TODO: Break up this giant object. 
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
          // Initial lead is 8D...
          // ...unless it was passed, but I'm not handling passing yet. 
          assert(hand.contains(Card(R8, Diamond)))
          Set(Card(R8,Diamond))
        } else {  // Player in lead can play anything.
          hand.toSet
        }
      } else {
        val ledSuit = table(leadPos).get.suit
        val inSuit = hand.filter(c => c.suit == ledSuit)
        if (inSuit.isEmpty) hand.toSet
        else inSuit
      }
    }
  }

  def displayView(v:View) = {
    def cardRepr(cardOpt:Option[Card], wasLed:Boolean, canWin:Boolean) = {
      val fmtString = (wasLed, canWin) match {
        case (true, true)   => " .(%-4s). "
        case (true, false)  => "  .%-4s.  "
        case (false, true)  => "  (%-4s)  "
        case (false, false) => "   %-4s   "
      }
      fmtString.format(cardOpt match {
        case Some(card) => card.toString
        case None       => "-?-"
      })
    }

    def youAreHere() = 
      if (Config.displayMode.isUnicode) "↓You↓" else ".You."

    val ptsLeft = 120 - v.pointsTaken.sum
    val ptsInTrick = null

    val leadIdx = v.leadPos
    val winningIdx = TrickLogic.winner(v.table, leadIdx)

    val tableStr =
      DisplayTable(
        Vector(Vector("Trick #%2d".format(v.trickNumber)),
               Vector.fill(5)("").updated(v.playerId + 1, youAreHere),
               "Pts. taken:" +: (0 to 3).map(i => v.pointsTaken(i).toString) :+ "(%d left)".format(ptsLeft),
               "On table:  " +: (0 to 3).map(i => cardRepr(v.table(i), i == leadIdx, Some(i) == winningIdx))))
    println(tableStr)
    println("Your hand: " + v.hand)
  }

  case class State (
    strategies:Vector[TrickStrategy],
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

  def fourRandoms(rng: Random) = Vector.fill(4)(new RandomLegalMove(rng))

  def threeRandomsAndConsole(rng: Random) = (new FromConsole) +: Vector.fill(3)(new RandomLegalMove(rng))

  def newRound(rng:Random = DefaultRNG)(implicit strategies:Vector[TrickStrategy] = fourRandoms(rng)):State = {
    val hands = Deck.deal(rng).map(new Hand(_))
    State(strategies = strategies,
          rng = rng,
          trickNumber = 0, leadPos = 0, pointsTaken = Vector.fill(4)(0),
          table = Vector.fill(4)(None), hands = hands)
  }

  trait TrickStrategy extends Function1[View, Card]
  // Note: actual strategies will have a memory of the round & game. A later
  // improvement. 

  class RandomLegalMove(private val rng:Random) extends TrickStrategy {
    def apply(v:View):Card = {
      val legalMoves = v.legalMoves.toArray
      val idx = (rng.nextDouble() * legalMoves.length).toInt
      legalMoves(idx)
    }
  }

  class FromConsole extends TrickStrategy {
    def apply(v:View) = {
      displayView(v)
      val legalMoves = v.legalMoves
      assert(!legalMoves.isEmpty)

      // TODO: make this less ugly. 
      def loop():Card = {
        println(legalMoves.mkString("Legal plays: ", "  ", ""))
        if (Config.noviceMode) {
          println(legalMoves.map(card => "%3d".format(PointValue(card))).mkString(
            "             ", " ", ""))
        }
        print("Your play: ")
        val humanInput = Console.readLine
        val cardOpt = 
          try {
            Some(CardUtils.cardOfHumanInput(humanInput))
          } catch {
            case (e:StupidHuman) => {
              println("That's not a card.")
              None
            }
          }
        cardOpt match {
          case None       => {
            println("That's not a legal play.")
            loop()
          }
          case Some(card) => {
            if (legalMoves.contains(card)) card
            else loop()
          }
        }
      }
      loop()
    }
  }

  private def cardRepr(card:Card, wasLed:Boolean, winner:Boolean) = {
    (wasLed, winner) match {
      case (true, true)   => " .[%-4s]. ".format(card)
      case (true, false)  => "  .%-4s.  ".format(card)
      case (false, true)  => "  [%-4s]  ".format(card)
      case (false, false) => "   %-4s   ".format(card)
    }
  }

  def displayTrick(cards:Vector[Card], trickNumber:Int, leadIdx:Int, winnerIdx:Int, oldCount:Int, ptsWon:Int) = {
    "Trick #%2d:   %10s%10s%10s%10s || player #%d, %2d pts. (%3d -> %3d)".format(
      trickNumber,
      cardRepr(cards(0), leadIdx == 0, winnerIdx == 0),
      cardRepr(cards(1), leadIdx == 1, winnerIdx == 1),
      cardRepr(cards(2), leadIdx == 2, winnerIdx == 2),
      cardRepr(cards(3), leadIdx == 3, winnerIdx == 3),
      winnerIdx, ptsWon, oldCount, oldCount + ptsWon)
  }

  def isTerminal(rs:State):Boolean = {
    rs.trickNumber > 13
  }

  def step(rs:State):State = {
    if (rs.trickNumber == 0) {
      // If trick# = 0, find 8D and that person leads.
      val newLeadPos = rs.hands.indexWhere(_.contains(Card(R8, Diamond)))
      rs.copy(trickNumber = 1, leadPos = newLeadPos)
    } else {
      val playerOrder = (0 to 3).map(i => (rs.leadPos + i) % 4)
      val activePlayerOpt = playerOrder.find(i => rs.table(i) == None)
      activePlayerOpt match {
        case Some(activePlayer) => {  // Trick not yet complete.
          val card = rs.strategies(activePlayer).apply(rs.view(activePlayer))
          rs.copy(table = rs.table.updated(activePlayer, Some(card)),
                  hands = rs.hands.updated(activePlayer, rs.hands(activePlayer).remove(card)))
        }
        case None => {  // Trick is complete. Resolve it.
          val winnerIdx = TrickLogic.winner(rs.table, rs.leadPos).get
          val trickValue = TrickLogic.trickValue(rs.table)
          if (Config.displayTricks) {
            println(displayTrick(cards       = rs.table.map(_.get), 
                                 trickNumber = rs.trickNumber,
                                 leadIdx     = rs.leadPos,
                                 winnerIdx   = winnerIdx,
                                 ptsWon      = trickValue,
                                 oldCount    = rs.pointsTaken(winnerIdx)))
          }
          rs.copy(trickNumber = rs.trickNumber + 1,
                  table       = Vector(None, None, None, None),
                  leadPos     = winnerIdx,
                  // TODO: Find out if there's a more elegant way of applying a fn. to
                  // one elt. of a vector (e.g. that 'update' I always write in Clj.)
                  pointsTaken = rs.pointsTaken.updated(winnerIdx, 
                                                       rs.pointsTaken(winnerIdx) + trickValue))
        }
      }
    }
  }

  def playARound() = {
    var state = newRound()
    while (!isTerminal(state)) {
      state = step(state)
    }
    state
  }
}

object Ambition extends App {
  override def main(args:Array[String]) = {
    
  }
}
