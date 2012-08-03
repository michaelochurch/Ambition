import scala.util.Random

import CardUtils._
import Rank.{R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA}
import Suit.{Diamond, Spade, Heart, Club}

import java.io.{File, PrintWriter}

object Ambition {
  object Config {
    val displayMode = Unicode
    val coloringMode = FourColor
  }

  object PointValues {
    private[this] def isHonor(r:Rank.T) = r >= RJ

    private[this] def points(r:Rank.T, s:Suit.T):Int = {
      s match {
        case Club    => if (r == RK)            17 else 0
        case Diamond => if (r == R8)             8 else if (isHonor(r)) 3 else 1
        case Heart   => if (r == R2)             8 else if (isHonor(r)) 3 else 1
        case Spade   => if (r == R8 || r == RA)  8 else if (isHonor(r)) 5 else 2
      }
    }

    private[this] val pointValues = 
      Array.tabulate(4, 13) { (s, r) => points(Rank(r), Suit(s)) }

    def apply(rank:Rank.T, suit:Suit.T):Int = pointValues(suit.id)(rank.id)
    
    def apply(card:Card):Int = apply(card.rank, card.suit)
  }

  case class Trick(leadPos: Int, cards: Vector[Option[Card]]) {
    def activePlayer = {
      (0 to 3).map(i => (i + leadPos) % 4).find(cards(_) == None)
    }
    
    def isComplete = {
      cards.forall(_ != None)
    }
    
    def play(pos:Int, card:Card):Trick = {
      assert(cards(pos) == None)
      copy(cards = cards.updated(pos, Some(card)))
    }
    
    def ledSuit = {
      cards(leadPos) match {
        case Some(card) => card.suit
        case None       => assert(false)
      }
    }

    def pointValue = {
      cards.foldLeft(0) { (acc, cardOpt) =>
        cardOpt match {
          case Some(card) => acc + PointValues(card)
          case None       => acc
        }
      }
    }

    def winner = {
      val honor = cards.exists {
        case Some(c) => c.isHonor && c.suit == ledSuit
        case None    => false
      }

      (0 to 3).maxBy(i => cards(i) match {
        case Some(c) => {
          if (c.suit == ledSuit) {
            if (honor && c.rank == R2)
              100
            else
              c.rank.id
          } else -100
        }
        case None   => -100
      })
    }

    def csvLine = {
      "%s,%s,%s,%s,%s,%s,%s\n".format(
        cards(0).get.twoChar, cards(1).get.twoChar, cards(2).get.twoChar, cards(3).get.twoChar,
        leadPos, winner, pointValue)
    }
  }

  // Set of cards displayed in a useful order. 
  class CardSet(cards:Iterable[Card]) {
    private val cardSet = cards.toSet
    
    def toSet():Set[Card] = cardSet

    // TODO: Put coloring into this. 
    override def toString = 
      cardSet.toList.sortBy(c => (c.suit.id << 4) + (if (c.rank == R2) 15 else c.rank.id)).mkString("  ")

    def asciiRepr = cards.map(_.twoChar).mkString("-")

    def remove(card:Card) = {
      assert(cardSet.contains(card))
      new CardSet((cardSet - card).toSeq)
    }

    def csvLine = {
      val vec = {
        (asciiRepr +: (for (r <- CardUtils.Rank.values.toList;
                            s <- CardUtils.Suit.values.toList) yield (if (cardSet.contains(Card(r, s))) 1 else 0)))
      }
      vec mkString ("", ",", "\n")
    }
  }

  object CardSet {
    def apply(cards:Iterable[Card]) = new CardSet(cards)
  }

  implicit def cardSetToSet(x:CardSet) = x.toSet

  object DefaultRNG {
    val rng = new Random()
    def apply() = rng
  }

  // Corresponds to one player's view into the game. Players are permitted to
  // see the last trick but not whole round history (unless they remember it.)
  case class RoundView (
    playerId : Int,
    trickNumber : Int,
    pointsTaken : Vector[Int],
    lastTrick : Option[Trick],
    thisTrick : Trick,
    winnerSoFar : Int,
    hand : CardSet) {
   
    def legalMoves():CardSet = {
      val leadPos = thisTrick.leadPos
      if (leadPos == playerId) {
        if (trickNumber == 1) {
          // Initial lead is 8D...
          // ...unless it was passed, but I'm not handling passing yet. 
          assert(hand.contains(Card(R8, Diamond)))
          CardSet(Vector(Card(R8,Diamond)))
        } else {  // Player in lead can play anything.
          hand
        }
      } else {
        val ledSuit = 
          thisTrick.cards(leadPos) match {
            case None       => assert(false)
            case Some(card) => card.suit
          }
        val inSuit = hand.filter(c => c.suit == ledSuit)
        if (inSuit.isEmpty) hand
        else CardSet(inSuit)
      }
    }
  }

  // Right now, there are two TrickStrategies... (a) random legal play, and (b)
  // ask a human. 
  trait TrickStrategy extends Function1[RoundView, Card] {
    val id : String
  }

  class RandomLegalMove(private val rng:Random) extends TrickStrategy {
    val id = "2012/07/29-Random"
    def apply(v:RoundView):Card = {
      val legalMoves = v.legalMoves.toArray
      val idx = (rng.nextDouble() * legalMoves.length).toInt
      legalMoves(idx)
    }
  }

  class FromConsole extends TrickStrategy {
    val id = "2012/07/29-FromConsole"
    def apply(v:RoundView) = {
      throw new Exception("not implemented")
    }
  }

  // TODO: currently I represent "before trick play" with trickNumber = 0 and
  // "end of round" with 14. This is not incorrect but a bit unclear. 
  case class RoundState (
    strategies:Vector[TrickStrategy],
    rng : Random,
    trickNumber : Int,
    leadPos : Int,
    pointsTaken : Vector[Int],
    trickHistory : Vector[Option[Trick]],
    hands : Vector[CardSet]) {
    
    private def newTrick(leadPos:Int):Trick = {
      Trick(leadPos = leadPos,
            cards = Vector(None, None, None, None))
    }

    // fails if called before or after round. 
    def currentTrick = trickHistory(trickNumber - 1).get

    def activePlayer:Option[Int] =
      currentTrick.activePlayer

    def view(playerId:Int):RoundView = {
      val lastTrick =
        if (trickNumber < 2) None else trickHistory(trickNumber - 2)
      RoundView(playerId = playerId,
                trickNumber = trickNumber,
                pointsTaken = pointsTaken,
                lastTrick = lastTrick,
                thisTrick = currentTrick,
                winnerSoFar = currentTrick.winner,
                hand = hands(playerId))
    }
    
    def isTerminal():Boolean = trickNumber > 13

    def find8D():RoundState = {
      val newLeadPos = hands.indexWhere(_.contains(Card(R8, Diamond)))
      this.copy(trickNumber = 1, leadPos = newLeadPos,
                trickHistory = trickHistory.updated(0, Some(newTrick(newLeadPos))))
    }

    def playCard(pos:Int, card:Card):RoundState = {
      this.copy(trickHistory = trickHistory.updated(trickNumber - 1, Some(currentTrick.play(pos, card))),
                hands        = hands.updated(pos, hands(pos).remove(card)))
    }

    def resolveTrick:RoundState = {
      val trickWinner = currentTrick.winner
      val trickValue  = currentTrick.pointValue
      this.copy(trickNumber  = trickNumber + 1,
                leadPos      = trickWinner,
                pointsTaken  = pointsTaken.updated(trickWinner, pointsTaken(trickWinner) + trickValue),
                trickHistory = {
                  if (trickNumber < 13)
                    trickHistory.updated(trickNumber, Some(newTrick(trickWinner)))
                  else trickHistory})
    }

    def step():RoundState = {
      if (trickNumber == 0) {
        find8D()
      } else {
        activePlayer match {
          case Some(playerId) => {
            val card = strategies(playerId).apply(view(playerId))
            playCard(playerId, card)
          }
          case None => {
            resolveTrick
          }
        }
      }
    }  
  }

  def fourRandoms(rng:Random) = 
    Vector.fill(4)(new RandomLegalMove(rng))

  def startRound(rng:Random = DefaultRNG())(
    implicit strategies:Vector[TrickStrategy] = fourRandoms(rng)):RoundState = {
    val hands = Deck.deal(rng).map(new CardSet(_))
    RoundState(strategies = strategies,
               rng = rng,
               trickNumber = 0, 
               leadPos = 0, 
               pointsTaken = Vector.fill(4)(0),
               trickHistory = Vector.fill(13)(None), 
               hands = hands)
  }

  def startRoundWithHands(hands:Vector[CardSet])(
    implicit strategies:Vector[TrickStrategy] = fourRandoms(DefaultRNG())):RoundState = {
    RoundState(strategies = strategies,
               rng = null, // won't be used.
               trickNumber = 0,
               leadPos = 0,
               pointsTaken = Vector.fill(4)(0),
               trickHistory = Vector.fill(13)(None),
               hands = hands)
  }

  def playARound(initState:RoundState = startRound()) = {
    var state = initState
    while (!state.isTerminal) {
      state = state.step
    }
    state
  }

  def scoreRound(state:RoundState):Vector[Int] = {
    val scores = state.pointsTaken
    val most   = scores.max
    val slam   = most >= 75
    val nils   = scores.count(_ == 0)
    val under  = scores.exists(x => x < 15 && x > 0)
    val nilValue = 
      (slam, nils, under) match {
        case (true, (2 | 3), _    ) =>  0
        case (true, 1, false)       =>  0
        case (true, 1, true)        => 30
        case (false, 2, _)          => 15
        case (false, 1, _)          => 30
        case _                      => 30 // irrelevant.
      }
    scores.map(x => 
      if      (x >= 75)            60  // Slam
      else if (!slam && x == most)  0  // Overstrike
      else if (x == 0)       nilValue  // Nil   
      else if (x < 15)              x  // Understrike
      else                          x)
  }

  def handScoringExperiment(nTrials:Int, nHandSets:Int, rng:Random) = {
    val handsWriter = new PrintWriter (new File ("hands.txt"))
    val tricksWriter = new PrintWriter (new File ("tricks.txt"))
    val resultsWriter = new PrintWriter (new File ("results.txt"))

    def statRound(handSetId:Int, hands:Vector[CardSet]) = {
      val initState = startRoundWithHands(hands)
      val endState = playARound(initState)
//      for (Some(trick) <- endState.trickHistory) {
//        tricksWriter.write(trick.csvLine)
//      }
      tricksWriter.write(endState.trickHistory.map(trick => trick.get.pointValue).mkString("", ",", "\n"))
      val ptsTaken = endState.pointsTaken
      val ptsScored = scoreRound(endState)
      val nils   = ptsTaken.map(x => if (x == 0) 1 else 0)
      val slams  = ptsTaken.map(x => if (x >= 75) 1 else 0)
      val unders = ptsTaken.map(x  => if (x < 15) 1 else 0)
      val overs  = ptsScored.zip(ptsTaken).map {
        case (s, t) => if (s == 0 && t != 0) 1 else 0
      }
      val relScore = {
        val sumScored = ptsScored.sum
        ptsScored.map(4 * _ - sumScored)
      }
      val csvList = handSetId +: (ptsTaken ++ ptsScored ++ nils ++ slams ++ unders ++ overs ++ relScore)
      val csvLine = csvList mkString ("", ",", "\n")
      resultsWriter.write(csvLine)
    }

    def statHandSet(handSetId:Int) = {
      println("stating handSet# " + (handSetId + 1))
      val hands = Deck.deal(rng).map(new CardSet(_))
      val csvLines = hands.zipWithIndex.map { 
        case (hand, idx) => "%d,%d,".format(handSetId, idx) + hands(idx).csvLine
      }
      for (line <- csvLines) {
        handsWriter.write(line)
      }
      for (i <- 1 to nTrials) {
        statRound(handSetId, hands)
      }
    }
 
    for (i <- 1 to nHandSets) {
      statHandSet(i)
    }
    handsWriter.close()
    tricksWriter.close()
    resultsWriter.close()
  }

  def main(args:Array[String]) = {
    handScoringExperiment(1600, 1000, DefaultRNG())
  }
}
