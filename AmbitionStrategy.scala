import AmbitionCommon._
import PassingStrategy._
import TrickStrategy._
import Utils._

import scala.util.Random

trait AmbitionStrategy {
  val passing : PassingStrategy
  val trickPlaying : TrickStrategy
  def notify(x:Any): Unit
}

object ConsolePlayer extends AmbitionStrategy {
  val passing = PassFromConsole
  val trickPlaying = TrickPlayFromConsole
  def notify(x:Any) = {
    x match {
      case Ambition.RoundComplete(trickHistory, roundHistory, playerId, 
                                  newStrikes, newScores) => {
        AmbitionDisplay.printEndOfRound(trickHistory, roundHistory, playerId) 
      }
      case _ => failwith("unsupported message type")
    }
  }
}

class RandomPlayer(rng:Random) extends AmbitionStrategy {
  val passing = new RandomLegalPass(rng)
  val trickPlaying = new RandomLegalMove(rng)
  def notify(x:Any) = ()
}
