import scala.util.Random

import CardUtils._
import Rank.{R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA}
import Suit.{Diamond, Spade, Heart, Club}

import java.io.{File, PrintWriter}

object Ambition {
  import Utils._
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

  case class TrickResult(winner: Int, oldScore: Int, newScore: Int)

  case class Trick(number: Int, leadPos: Int, cards: Vector[Option[Card]],
                   finished: Option[TrickResult]) {
    def activePlayer = {
      (0 to 3).map(i => (i + leadPos) % 4).find(cards(_) == None)
    }
    
    def isComplete = {
      cards.forall(_ != None)
    }
 
    def markFinished(previousScore:Int) = {
      this.copy(finished = Some(TrickResult(winner = winner, 
                                            oldScore = previousScore, 
                                            newScore = previousScore + pointValue)))
    }

    def isEmpty = {
      cards.forall(_ == None)
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
        case None   => (if (i == leadPos) -100 else -200)
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

    def toVector():Vector[Card] = 
      (Vector() ++ cards).sortBy(c => (c.suit.id << 4) + (if (c.rank == R2) 15 else c.rank.id))

    override def toString = 
      toVector() mkString " "

    def asciiRepr = cards.map(_.twoChar).mkString("-")

    def remove(card:Card) = {
      assert(cardSet.contains(card))
      new CardSet((cardSet - card).toSeq)
    }

    def ++(cards:Iterable[Card]) = {
      new CardSet(cardSet ++ cards)
    }

    def --(cards:Iterable[Card]):CardSet = {
      if (cards.isEmpty) this
      else (this remove cards.head) -- cards.tail
    }

    // For data collection. 
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
    
    def ofString(s:String) = new CardSet(s.split(" ").map(CardUtils.cardOfHumanInput(_)))
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
    d8WasPassed : Boolean,
    roundNumber : Int,
    passingType : PassingType,
    trickNumber : Int,
    pointsTaken : Vector[Int],
    lastTrick : Option[Trick],
    thisTrick : Option[Trick],
    hand : CardSet) {
   
    def legalMoves():CardSet = {
      val leadPos = thisTrick.get.leadPos
      if (leadPos == playerId) {
        if (trickNumber == 1) {
          // Initial lead is 8D...
          // ...unless it was passed, but I'm not handling passing yet. 
          assert(hand.contains(Card(R8, Diamond)))
          if (d8WasPassed) println("WARNING: passed 8D not handled correctly yet.")
          CardSet(Vector(Card(R8,Diamond)))
        } else {  // Player in lead can play anything.
          hand
        }
      } else {
        val ledSuit = 
          thisTrick.get.cards(leadPos) match {
            case None       => assert(false)
            case Some(card) => card.suit
          }
        val inSuit = hand.filter(c => c.suit == ledSuit)
        if (inSuit.isEmpty) hand
        else CardSet(inSuit)
      }
    }
  }

  sealed trait PassingType
  case object PassLeft extends PassingType
  case object PassRight extends PassingType
  case object PassAcross extends PassingType
  case object ScatterPass extends PassingType
  
  object PassingType {
    def apply(roundNumber:Int) = {
      (roundNumber % 4) match {
        case 1 => PassLeft
        case 2 => PassRight
        case 3 => PassAcross
        case 0 => ScatterPass
      }
    }
  }

  sealed trait RoundTime
  case class AwaitingPass(playerId:Int) extends RoundTime
  case object AfterPassing extends RoundTime
  case class OnTrick(number:Int) extends RoundTime
  case object Finished extends RoundTime

  // TODO: currently I represent "before trick play" with trickNumber = 0 and
  // "end of round" with 14. This is not incorrect but a bit unclear. 
  case class RoundState (
    roundNumber : Int,
    strategies:Vector[AmbitionStrategy],
    passingType : PassingType,
    d8WasPassed : Boolean = false,
    passingSpace : Vector[Vector[Card]] = Vector.fill(4, 3)(null),
    rng : Random,
    roundTime : RoundTime,
    leadPos : Int,
    pointsTaken : Vector[Int],
    trickHistory : Vector[Option[Trick]],
    hands : Vector[CardSet]) {
    
    def trickNumber():Int = {
      roundTime match {
        case OnTrick(n) => n
        case Finished                       => 14 // backward compatibility.
        case (AwaitingPass(_) | AfterPassing) => 0
        // TODO: trickNumber shouldn't be called except during trick play. Or it should 
        // return an option. 
      }
    }

    private def newTrick(number:Int, leadPos:Int):Trick = {
      Trick(number = number,
            leadPos = leadPos,
            cards = Vector(None, None, None, None),
            finished = None)
    }

    // TODO: when trickNumber return is changed to Option[Int], this will need to change too. 
    def currentTrickOption : Option[Trick] = {
      if (trickNumber <= 0 || trickNumber >= 14) None
      else trickHistory(trickNumber - 1)
    }

    // fails if called before or after round. 
    def currentTrick : Trick = currentTrickOption.get

    def activePlayer : Option[Int] =
      currentTrick.activePlayer

    def view(playerId:Int):RoundView = {
      val lastTrick =
        if (trickNumber < 2) None else trickHistory(trickNumber - 2)
      RoundView(playerId = playerId,
                d8WasPassed = d8WasPassed,
                roundNumber = roundNumber,
                passingType = passingType,
                trickNumber = trickNumber,
                pointsTaken = pointsTaken,
                lastTrick = lastTrick,
                thisTrick = currentTrickOption,
                hand = hands(playerId))
    }
    
    def isTerminal():Boolean = roundTime == Finished

    def find8D():RoundState = {
      val newLeadPos = hands.indexWhere(_.contains(Card(R8, Diamond)))
      this.copy(roundTime = OnTrick(1), leadPos = newLeadPos,
                trickHistory = trickHistory.updated(0, Some(newTrick(1, newLeadPos))))
    }

    def playCard(pos:Int, card:Card):RoundState = {
      this.copy(trickHistory = trickHistory.updated(trickNumber - 1, Some(currentTrick.play(pos, card))),
                hands        = hands.updated(pos, hands(pos).remove(card)))
    }

    def markTrickFinished(trickNumber:Int, previousScore:Int):RoundState = {
      val trick = trickHistory(trickNumber - 1).get
      this.copy(trickHistory = trickHistory.updated(trickNumber - 1, Some(trick.markFinished(previousScore))))
    }

    def resolveTrick:RoundState = {
      val trickWinner  = currentTrick.winner
      val trickValue   = currentTrick.pointValue
      val newRoundTime = roundTime match {
        case OnTrick(n) => if (n < 13) OnTrick(n + 1) else Finished
        case _          => throw new Exception("assertFalse")
      }
      this.copy(roundTime    = newRoundTime,
                leadPos      = trickWinner,
                pointsTaken  = pointsTaken.updated(trickWinner, pointsTaken(trickWinner) + trickValue),
                trickHistory = {
                  if (trickNumber < 13)
                    trickHistory.updated(trickNumber, Some(newTrick(trickNumber + 1, trickWinner)))
                  else trickHistory}).markTrickFinished(trickNumber, previousScore = pointsTaken(trickWinner))
    }

    // Left off here. 
    private def performPass(fromPlayer: Int, passingType: PassingType,
                            cards:(Card, Card, Card)) : RoundState = {
      val cardsVector = Vector(cards._1, cards._2, cards._3)
      val newPassingSpace = passingType match {
        case PassLeft => passingSpace.updated((fromPlayer + 3) % 4, cardsVector)
        case PassRight => passingSpace.updated((fromPlayer + 1) % 4, cardsVector)
        case PassAcross => passingSpace.updated((fromPlayer + 2) % 4, cardsVector)
        case ScatterPass => throw new Exception("not implemented")
      }
      this.copy(roundTime = AwaitingPass(fromPlayer + 1),
                passingSpace = newPassingSpace,
                hands     = hands.updateWith(fromPlayer) { cardSet => 
                  cardSet -- cardsVector})
    }

    private def passIntoHands(hands: Vector[CardSet], passingSpace: Vector[Vector[Card]]) = {
      hands.zip(passingSpace).map { case (hand, passedCards) => hand ++ passedCards }
    }

    def completePass : RoundState = {
      val d8WasPassed = passingSpace.exists(_.contains(Card(R8, Diamond)))
      val newHands = passIntoHands(hands, passingSpace)
      this.copy(roundTime = AfterPassing,
                d8WasPassed = d8WasPassed,
                hands = newHands)
    }

    def step():RoundState = {
      roundTime match {
        case AwaitingPass(playerId) => {
          if (playerId == 4)
            // TODO: This shouldn't use AwaitingPass(4), since 4 is a nonexistent player, for
            // "ready to complete pass". 
            completePass
          else {
            val cards = strategies(playerId).passing(view(playerId))
            performPass(playerId, passingType, cards)
          }
        }
        case AfterPassing => find8D()
        case OnTrick(_)   => {
          activePlayer match {
            case Some(playerId) => {
              val playerView = view(playerId)
              val card = strategies(playerId).trickPlaying(playerView)
              playCard(playerId, card)
            }
            case None => {
              resolveTrick
            }
          }
        }
        case Finished   => throw new Exception("step called on terminal RoundState")
      }
    }
  }

  def fourRandoms(rng:Random) = 
    Vector.fill(4)(new RandomPlayer(rng))

  def threeRandomsPlusConsole(rng:Random) = 
    ConsolePlayer +: Vector.fill(3)(new RandomPlayer(rng))

  def startRound(rng:Random = DefaultRNG(), roundNumber:Int = 1)(
    implicit strategies:Vector[AmbitionStrategy] = fourRandoms(rng)):RoundState = {
    val hands = Deck.deal(rng).map(new CardSet(_))
    RoundState(strategies = strategies,
               rng = rng,
               roundNumber = roundNumber,
               passingType = PassingType(roundNumber),
               roundTime = AwaitingPass(0),
               leadPos = 0, 
               pointsTaken = Vector.fill(4)(0),
               trickHistory = Vector.fill(13)(None), 
               hands = hands)
  }
  
  def startRoundWithHands(hands:Vector[CardSet])(
    implicit strategies:Vector[AmbitionStrategy] = fourRandoms(DefaultRNG())):RoundState = {
    RoundState(strategies = strategies,
               rng = null, // won't be used.
               roundNumber = 0,
               passingType = null,
               roundTime = AfterPassing,
               leadPos = 0,
               pointsTaken = Vector.fill(4)(0),
               trickHistory = Vector.fill(13)(None),
               hands = hands)
  }

  val demoHands = Vector(CardSet.ofString("4d qd 2d 3s 5s ks 2s 7h ah 2h tc kc 2c"),
                         CardSet.ofString("6d 7d 8d ts qs 5h th jh 3c 5c 6c qc ac"),
                         CardSet.ofString("3d 5d kd ad 4s 7s js as 3h 8h 9h 9c jc"),
                         CardSet.ofString("9d td jd 6s 8s 9s 4h 6h qh kh 4c 7c 8c"))                                          

  // Gives the demo. player a nice-looking hand. 
  def demoRoundState() = {
    val rng = DefaultRNG()
    RoundState(strategies = threeRandomsPlusConsole(rng),
               rng        = null,
               passingType = null,
               roundNumber = 0,
               leadPos = 0,
               roundTime = AfterPassing,
               pointsTaken = Vector.fill(4)(0),
               trickHistory = Vector.fill(13)(None),
               hands = demoHands)
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

  def main(args:Array[String]) = {
    val rng = DefaultRNG()
    val initRoundState =
      startRound(rng)(threeRandomsPlusConsole(rng))
    val endRoundState = playARound(initRoundState)
    AmbitionDisplay.printTrickHistory(endRoundState)
    //handScoringExperiment(1600, 1000, DefaultRNG())
  }
}
