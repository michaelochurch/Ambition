import PassingStrategy._
import TrickStrategy._

import scala.util.Random

trait AmbitionStrategy {
  val passing : PassingStrategy
  val trickPlaying : TrickStrategy
}

object ConsolePlayer extends AmbitionStrategy {
  val passing = PassFromConsole
  val trickPlaying = TrickPlayFromConsole
}

class RandomPlayer(rng:Random) extends AmbitionStrategy {
  val passing = new RandomLegalPass(rng)
  val trickPlaying = new RandomLegalMove(rng)
}
