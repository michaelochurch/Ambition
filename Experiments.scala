import java.io.{File, PrintWriter}
import scala.util.Random

object Experiments {
  import Ambition._
  def handScoringExperiment(nTrials:Int, nHandSets:Int, rng:Random) = {
    val handsWriter = new PrintWriter (new File ("hands.txt"))
    val tricksWriter = new PrintWriter (new File ("tricks.txt"))
    val resultsWriter = new PrintWriter (new File ("results.txt"))

    def statRound(handSetId:Int, hands:Vector[CardSet]) = {
      val initState = startRoundWithHands(hands)
      val endState = playARound(initState)
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
}
