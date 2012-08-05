object AmbitionDisplay {
  import Display._
  import CardUtils._
  import Rank.{R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA}
  import Suit.{Club, Diamond, Heart, Spade}
  
  sealed trait CardDisplayMode
  case object ThreeCharAscii extends CardDisplayMode
  case object TwoCharAscii extends CardDisplayMode
  case object Unicode extends CardDisplayMode

  sealed trait CardColoringMode
  case object FourColor extends CardColoringMode
  case object TwoColor extends CardColoringMode
  case object OneColor extends CardColoringMode

  case class Config(cardDisplayMode : CardDisplayMode,
                    cardColoringMode : CardColoringMode)

  val DefaultConfig = new Config(Unicode, FourColor)

  def cardString(card:Card, displayMode:CardDisplayMode):String = {
    displayMode match {
      case ThreeCharAscii => "%s%c".format(Rank.TwoCharReprs(card.rank.id), 
                                           Suit.AsciiReprs(card.suit.id))
      case Unicode        => "%s%c ".format(Rank.TwoCharReprs(card.rank.id), 
                                            Suit.Reprs(card.suit.id))
      case TwoCharAscii   => "%c%c".format(Rank.OneCharReprs(card.rank.id), 
                                           Suit.AsciiReprs(card.suit.id))
    }
  }

  def cardColor(card: Card, coloringMode:CardColoringMode):Option[Display.Color] = {
    coloringMode match {
      case FourColor => {
        card.suit match {
          case Club    => None  // Black on white background; white on black.
          case Diamond => Some(Display.Blue(true))
          case Heart   => Some(Display.Red(false))
          case Spade   => Some(Display.Green(false))
        }
      }
      case TwoColor => {
        card.suit match {
          case (Diamond | Heart) => Some(Display.Red(false))
          case _ => None
        }
      }
      case OneColor => None
    }
  }

  def displayCard(card:Card, config:Config = DefaultConfig):MarkupSegment = {
    MarkupSegment(cardString(card, config.cardDisplayMode),
                  cardColor(card, config.cardColoringMode))
  }

  def displayNoCard(config:Config = DefaultConfig):MarkupSegment = {
    val cardStr = config.cardDisplayMode match {
      case Unicode        => "-??-"
      case ThreeCharAscii => "-?-"
      case TwoCharAscii   => "??"
    }
    MarkupSegment(cardStr, None)
  }

  def rowOfTrick(trick:Ambition.Trick, config:Config = DefaultConfig):MarkupRow = {
    def cardCell(cardOpt:Option[Card], lead:Boolean, winner:Boolean) = {
      val (left, right) = {
        (lead, winner) match {
          case (true, true)   => (".[", "].")
          case (true, false)  => (" .", ". ")
          case (false, true)  => (" [", "] ")
          case (false, false) => ("  ", "  ")
        }
      }
      val center = cardOpt match {
        case Some(card) => displayCard(card, config)
        case None       => displayNoCard(config)
      }
      MarkupCell(Center, MarkupSegment(left), center, MarkupSegment(right))
    }
    val winnerPos = if (trick.isEmpty) -1 else trick.winner
    val trickNumberCell = MarkupCell(Right, "%2d.".format(trick.number))
    val pointsCell = MarkupCell(Center, 
                                MarkupSegment("("), 
                                MarkupSegment("%3d".format(trick.pointValue), Some(Cyan(true))),
                                if (trick.pointValue != 1) 
                                  MarkupSegment(" pts. )")
                                else MarkupSegment(" pt. ) "))
    val cardCells = trick.cards.zipWithIndex map { 
      case (cardOpt, idx) => 
        cardCell(cardOpt, idx == trick.leadPos, idx == winnerPos)
    }
    val cells = trickNumberCell +: (cardCells :+ pointsCell)
    DataRow(cells:_*)
  }

  def topTableOfView(v:Ambition.RoundView, config:Config = DefaultConfig) = {
    val youAreHereSym = config.cardDisplayMode match {
      case Unicode => "↓You ↓"
      case ThreeCharAscii => "You"
      case TwoCharAscii => "You "
    }
    val youAreHereCell = MarkupCell(Center, MarkupSegment(youAreHereSym, Some(Yellow(true))))
    val youAreHereCells = Vector.fill(5)(MarkupCell(Center, MarkupSegment(""))).updated(
      v.playerId + 1, youAreHereCell)
    val youAreHereRow = DataRow(youAreHereCells:_*)
    def pointsTakenRow(pointsTaken:Vector[Int]) = {
      val pointsLeft = 120 - pointsTaken.sum
      val mostPts    = pointsTaken.max
      val scoreCells = pointsTaken.map(pts => {
        val color = {
          if (pts >= 75)
            Some(Magenta(true))
          else if ((pts == 0) || (pts > 15 && ((pts < mostPts) || (pts <= 30)))) 
            Some(Green(true))
          else Some(Red(false))
        }
        MarkupCell(Center, MarkupSegment("%2d".format(pts), color))
      })
      val cells = MarkupCell(Left, MarkupSegment("Taken", None)) +: 
        (scoreCells :+ MarkupCell(Left, 
                                  MarkupSegment("("),
                                  MarkupSegment("%3d".format(pointsLeft), Some(Cyan(true))),
                                  MarkupSegment(" left.)")))
      DataRow(cells:_*)
    }
   
    val pointsTaken = v.pointsTaken

    val lastTrickRows = v.lastTrick match {
      case None => Vector()
      case Some(lastTrick) => {
        val oldPointsTaken = pointsTaken.updated(lastTrick.winner,
                                                 pointsTaken(lastTrick.winner) - lastTrick.pointValue)
        Vector(LiteralString("Previous Trick:"),
               youAreHereRow,
               pointsTakenRow(oldPointsTaken),
               rowOfTrick(lastTrick),
               LiteralString(""),
               LiteralString(""))
      }
    }
    val thisTrickRows = Vector(LiteralString("Current Trick:"),
                               youAreHereRow,
                               pointsTakenRow(v.pointsTaken),
                               rowOfTrick(v.thisTrick))
    Table((lastTrickRows ++ thisTrickRows):_*)
  }

  def bottomTableOfView(v:Ambition.RoundView, config:Config = DefaultConfig) = {
    val legalPlays = v.legalMoves.toVector
    Table(DataRow( (MarkupCell(Left, MarkupSegment("Your hand: ")) +: v.hand.toVector.map(card => 
                    MarkupCell(Center, displayCard(card, config)))):_*),
          DataRow( (MarkupCell(Left, MarkupSegment("Legal plays: ")) +: legalPlays.map(card => 
                    MarkupCell(Center, displayCard(card, config)))):_*),
          DataRow( (MarkupCell(Left, MarkupSegment("")) +: legalPlays.map(card => 
                    MarkupCell(Center, MarkupSegment("%2d".format(Ambition.PointValues(card)), 
                                                     Some(Cyan(true)))))):_*))
  }

  def printView(v:Ambition.RoundView, config:Config = DefaultConfig) = {
    println(topTableOfView(v, config).display())
    println()
    println(bottomTableOfView(v, config).display())
  }

  def main(args:Array[String]) = {
    val t = Ambition.Trick(number = 5, leadPos = 2,
                           cards = Vector(Some(Card(R2, Heart)), None, 
                                          Some(Card(RJ, Heart)), Some(Card(R8, Spade))))

    println(Table(rowOfTrick(t)).display())
  }
}