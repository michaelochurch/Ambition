import Ambition._
import CardUtils._
import scala.util.Random

// Right now, there are two TrickStrategies... (a) random legal play, and (b)
// ask a human. 
trait TrickStrategy extends Function1[RoundView, Card] {
  val id : String
  def randomLegalMove(v:RoundView, rng:Random) = {
    val legalMoves = v.legalMoves.toArray
    val idx = (rng.nextDouble() * legalMoves.length).toInt
    legalMoves(idx)
  }
}

object TrickStrategy {
  class RandomLegalMove(private val rng:Random) extends TrickStrategy {
    val id = "2012/07/29-Random"
    def apply(v:RoundView):Card = {
      randomLegalMove(v, rng)
    }
  }
  
  object TrickPlayFromConsole extends TrickStrategy {
    val id = "2012/07/29-FromConsole"
    def apply(v:RoundView):Card = {
      Display.clearScreen()
      AmbitionDisplay.printView(v)
      
      def loop():Card = {
        print(" Your move?  ")
        val input = readLine()
        println()
        val move = 
          try {
            val card = CardUtils.cardOfHumanInput(input)
            if (v.legalMoves.contains(card)) {
              Some(card)
            } else {
              println(" I know what that card is but it's not a legal play.")
              println(" (1) You can only play a card that you hold in your hand.")
              println(" (2) If able to follow suit (play a card in led suit) you must.")
              println(" (3) You must lead a Diamond to the first trick, and the 8D if it was not passed to you.")
              println(" Legal input might look like this: " + randomLegalMove(v, DefaultRNG()).toString)
              println()
              None
            }
          } catch {
            case (e:IllegalArgumentException) => {
              println(" I don't know how to parse that as a card: " + input)
              println(" Legal input might look like this: " + randomLegalMove(v, DefaultRNG()).toString)
              println()
              None
            }
          }
        move match {
          case Some(card) => card
          case None       => loop()
        }
      }
      
      loop()
    }
  }
}
