import Ambition._
import CardUtils._
import scala.util.Random

trait PassingStrategy extends Function1[RoundView, (Card, Card, Card)] {
  val id : String
  def randomLegalPass(v: RoundView, rng:Random) = {
    val h = v.hand.toArray
    assert(h.length == 13)
    val a = Utils.randomSelect(rng, h, 3)
    (a(0), a(1), a(2))
  }
}

object PassingStrategy {
  class RandomLegalPass(rng:Random) extends PassingStrategy {
    val id = "2012/08/05-RandomPass"
    def apply(v:RoundView) = {
      randomLegalPass(v, rng)
    }
  }
 
  // TODO: this function's too long. Break it up. 
  object PassFromConsole extends PassingStrategy {
    val id = "2012/08/05-PassFromConsole"
    def apply(v:RoundView) = {
      Display.clearScreen()
      AmbitionDisplay.printViewForPassing(v)
      def loop():(Card, Card, Card) = {
        print(" Your pass? (Type :r to pass randomly.) ")
        val input = readLine()
        val move = 
          try {
            if (input == ":r")
              Some(randomLegalPass(v, DefaultRNG()))
            else {
              val words = input.split(" ")
              if (words.length != 3) {
                printf(" You must pass exactly 3 cards. That's %d.\n", words.length)
                None
              } else {
                val cards = words.map(CardUtils.cardOfHumanInput)
                if (cards.forall(v.hand.contains(_)))
                  Some((cards(0), cards(1), cards(2)))
                else {
                  println(" You can only pass cards that are in your hand.")
                  None
                }
              }
            }
          } catch {
            case (e:IllegalArgumentException) => {
              println(" I don't know how to parse this as a set of cards: " + input)
              None
            }
          }
        move match {
          case Some(cards) => cards
          case None        => {
            print(" This would be an example of a legal pass: ") 
            val (c0, c1, c2) = randomLegalPass(v, DefaultRNG())
            print(c0 + " " + c1 + " " + c2 + "\n")
            loop()
          }
        }
      }
      loop()
    }
  }
}
