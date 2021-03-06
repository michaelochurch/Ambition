object AmbitionDisplay {
  import AmbitionCommon._
  import CardUtils._
  import Display._
  import Rank.{R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK, RA}
  import Suit.{Club, Diamond, Heart, Spade}
  import Utils._
  
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
    val finished = trick.finished != None
    val pointsCell = {
      val segments = {
        Vector(MarkupSegment("("),
               MarkupSegment("%3d".format(trick.pointValue), Some(Cyan(true)))) ++
        (if (!finished) {
          if (trick.pointValue != 1) 
            Vector(MarkupSegment(" pts. )"))
          else Vector(MarkupSegment(" pt. ) "))
        } else {
          val Some(trickResult) = trick.finished
          Vector(if (trick.pointValue != 1) MarkupSegment(" pts. : ") else MarkupSegment(" pt.  : "),
                 MarkupSegment("%3d".format(trickResult.oldScore), Some(Cyan(true))),
                 MarkupSegment(" -> "),
                 MarkupSegment("%3d".format(trickResult.newScore), Some(Cyan(true))),
                 MarkupSegment(")"))
        })
      }
      MarkupCell(Left, segments:_*)
    }
    val cardCells = trick.cards.zipWithIndex map { 
      case (cardOpt, idx) => 
        cardCell(cardOpt, idx == trick.leadPos, idx == winnerPos)
    }
    val cells = trickNumberCell +: (cardCells :+ pointsCell)
    DataRow(cells:_*)
  }

  private def topTableOfView(v:Ambition.RoundView, config:Config = DefaultConfig) = {
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
          else if ((pts == 0) || (pts >= 15 && ((pts < mostPts) || (pts <= 30)))) 
            Some(Green(true))
          else Some(Red(false))
        }
        MarkupCell(Center, MarkupSegment("%2d".format(pts), color))
      })
      val cells = MarkupCell(Left, MarkupSegment("Taken", None)) +: 
        (scoreCells :+ MarkupCell(Left, 
                                  MarkupSegment("("),
                                  MarkupSegment("%3d".format(pointsLeft), Some(Cyan(true))),
                                  MarkupSegment(" left)")))
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
    assert(v.thisTrick != None)
    val thisTrickRows = Vector(LiteralString("Current Trick:"),
                               youAreHereRow,
                               pointsTakenRow(v.pointsTaken),
                               rowOfTrick(v.thisTrick.get))
    Table((lastTrickRows ++ thisTrickRows):_*)
  }

  private def bottomTableOfView(v:Ambition.RoundView, config:Config = DefaultConfig) = {
    val legalPlays = v.legalMoves.toVector
    Table(DataRow( (MarkupCell(Left, MarkupSegment("Your hand: ")) +: v.hand.toVector.map(card => 
                    MarkupCell(Center, displayCard(card, config)))):_*),
          LiteralString(""),
          DataRow( (MarkupCell(Left, MarkupSegment("Legal plays: ")) +: legalPlays.map(card => 
                    MarkupCell(Center, displayCard(card, config)))):_*),
          DataRow( (MarkupCell(Left, MarkupSegment("")) +: legalPlays.map(card => 
                    MarkupCell(Center, MarkupSegment("%2d".format(Ambition.PointValues(card)), 
                                                     Some(Cyan(true)))))):_*))
  }

  // TODO: rename to printViewForTrick
  def printView(v:Ambition.RoundView, config:Config = DefaultConfig) = {
    println(topTableOfView(v, config).display())
    println()
    println(bottomTableOfView(v, config).display())
  }

  def printViewForPassing(v:Ambition.RoundView, config:Config = DefaultConfig) = {
    val table = 
      Table(DataRow( (MarkupCell(Left, MarkupSegment("Your hand: ")) +: v.hand.toVector.map(card => 
        MarkupCell(Center, displayCard(card, config)))):_*),
            LiteralString(""),
            LiteralString(" Please select 3 cards to pass %s".format( 
              v.passingType match {
                case Ambition.PassLeft  => "to your LEFT."
                case Ambition.PassRight => "to your RIGHT."
                case Ambition.PassAcross => "ACROSS."
                case Ambition.ScatterPass => "SCATTERED (1st to left, 2nd across, 3rd right)."
              })))
    println(table.display())
  }

  // TODO: trick history table should single out the viewing player's tricks in some way. 
  // i.e. the "You are here" marker. 

  private def trickHistoryRows(
    trickHistory:Vector[Ambition.Trick], config:Config = DefaultConfig) = {
    
    trickHistory.map(trick => rowOfTrick(trick))
  }

  private def roundHistoryRows(roundHistory:Vector[(RoundResult, AllScores)], 
                                config:Config) = {
    def cellOfPlayerOutcome(ptsScored:Int, strike:Boolean, nil:Boolean, slam:Boolean) = {
      val segment = {
        (strike, nil, slam) match {
          case (false, false, false) => MarkupSegment(ptsScored.toString)
          case (true, false, false) => {
            if (ptsScored == 0) MarkupSegment("X", Some(Red(false)))
            else MarkupSegment("%2d (X)".format(ptsScored), Some(Red(true)))
          }
          case (false, true, _) => {
            MarkupSegment("%2d (N)".format(ptsScored), Some(Blue(true)))
          }
          case (true, true, _) => {
            MarkupSegment("0 (NX)", Some(Red(true)))
          }
          case (false, false, true) => {
            MarkupSegment("%-3d(S)".format(ptsScored), Some(Yellow(true)))
          }
        }
      }
      MarkupCell(Center, segment)
    }

    def topRowOfRoundResult(roundNumber:Int, roundResult:RoundResult) = {
      DataRow( (Vector(MarkupCell(Center, MarkupSegment("%2d.".format(roundNumber)))) ++
                (0 to 3).map(i => cellOfPlayerOutcome(roundResult.pointsScored(i),
                                                      roundResult.strikes(i),
                                                      roundResult.nils(i),
                                                      roundResult.slams(i)))):_*)
    }

    // TODO: include prior pts & strikes in RoundResult so there's less hackishness.
    // OR... what I should be doing is 
    def bottomRowOfRoundResult(newScores:Vector[Int], newStrikes:Vector[Int]) = {
      val data = newScores.zip(newStrikes)
      DataRow( (Vector(MarkupCell(Center, "")) ++ data.map {
        case (pts, strikes) => MarkupCell(Center, "%d/%3d".format(strikes, pts))
      }):_*)
    }

    // Main body of roundHistory table creation. 
    roundHistory.zipWithIndex.flatMap {
      case ((roundResult, allScores), idx) => {
        Vector(LiteralString(""),
               topRowOfRoundResult(idx + 1, roundResult),
               Bar('-', " ", " ", " "),
               bottomRowOfRoundResult(allScores.points, allScores.strikes))
      }
    }
  }

  def endOfRoundTable(trickHistory:Vector[Ambition.Trick],
                      roundHistory:Vector[(RoundResult, AllScores)],
                      playerId:Int, 
                      config:Config = DefaultConfig) = {
    val allRows = {
      Vector(LiteralString(" End of Round #%d".format(roundHistory.length)),
             LiteralString(" Trick history:")) ++
      trickHistoryRows(trickHistory, config) ++
      Vector(LiteralString(""), LiteralString(" Round history:")) ++
      roundHistoryRows(roundHistory, config)      
    }
    Table(allRows:_*)
  }

  // TODO: include the "you are here" marker. 
  def printEndOfRound(trickHistory:Vector[Ambition.Trick],
                      roundHistory:Vector[(RoundResult, AllScores)],
                      playerId:Int,
                      config:Config = DefaultConfig) = {
    println(endOfRoundTable(trickHistory, roundHistory, playerId, config).display())
    // TODO: abstract this out. 
    println(" press enter to continue...")
    readLine()
  }

  def main(args:Array[String]) = {
    val t = Ambition.Trick(number = 5, leadPos = 2, finished = None,
                           cards = Vector(Some(Card(R2, Heart)), None, 
                                          Some(Card(RJ, Heart)), Some(Card(R8, Spade))))

    println(Table(rowOfTrick(t)).display())
  }
}
