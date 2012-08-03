object Display {
  sealed trait Color
  case class White(bold:Boolean = false) extends Color
  case class Red(bold:Boolean = false) extends Color
  case class Cyan(bold:Boolean = false) extends Color
  case class Green(bold:Boolean = false) extends Color
  case class Blue(bold:Boolean = false) extends Color
  case class Yellow(bold:Boolean = false) extends Color
  case class Magenta(bold:Boolean = false) extends Color
  case class Black(bold:Boolean = false) extends Color
  
  def textColor(c:Color) = {
    c match {
      case White(true) => Console.BOLD + Console.WHITE
      case White(false) => Console.WHITE
      case Red(true) => Console.BOLD + Console.RED
      case Red(false) => Console.RED
      case Cyan(true) => Console.BOLD + Console.CYAN
      case Cyan(false) => Console.CYAN
      case Green(true) => Console.BOLD + Console.GREEN
      case Green(false) => Console.GREEN
      case Blue(true) => Console.BOLD + Console.BLUE
      case Blue(false) => Console.BLUE
      case Yellow(true) => Console.BOLD + Console.YELLOW
      case Yellow(false) => Console.YELLOW
      case Magenta(true) => Console.BOLD + Console.MAGENTA
      case Magenta(false) => Console.MAGENTA
      case Black(true) => Console.BOLD + Console.BLACK
      case Black(false) => Console.BLACK
    }
  }
  
  def colorById(id:Int):Color = {
    val bold = (id % 16) >= 8
    (id % 8) match {
      case 0 => Black(bold)
      case 1 => Red(bold)
      case 2 => Green(bold)
      case 3 => Yellow(bold)
      case 4 => Blue(bold)
      case 5 => Magenta(bold)
      case 6 => Cyan(bold)
      case 7 => White(bold)
    }
  }
  
  def colored(text:String, color:Color) = {
    textColor(color) + text + Console.RESET
  }
  
  def main(args:Array[String]) = {
    for (i <- 0 to 15) {
      println(colored("%02d".format(i), colorById(i)))
    }
  }
}

// This is what client code will look like...

//     def pointsTable(cardDisplayMode:CardDisplayMode = Config.displayMode,
//                     cardColoringMode:CardColoringMode = Config.coloringMode) = {
//       import Display._
//       import TableBuildingImplicits._

//       val suitRow =
//         if (cardDisplayMode == Unicode) 
//           Row("", "♦",  "♠",  "♥",  "♣")
//         else Row("", "D", "S", "H", "C")

//       def rankRow(rankId:Int) = 
//         TableRow(Cell("R:", Rank.TwoCharReprs(rankId)) +:
//                  (0 to 3).map(i => Cell("R:", pointValues(i)(rankId).toString)))
      
//       val rows = {
//         (Bar('-', "---", "--", "--") +:
//          suitRow +:
//          Bar('-', "-+-", "+-", "-+") +:
//          (12 to 9 by -1).map(rankRow(_)) :+
//          Bar('-', "-+-", "+-", "-+")) ++
//         ((8 to 0 by -1).map(rankRow(_)) :+
//          Bar('-', "---", "--", "--"))
//       }

//       Table(rows:_*)
//     }
    
//     def displayPointsTable() = {
//       println(pointsTable().displayString(Display.Table.Config(
//         rowLeft = "| ", rowRight = " |", sep = " | ",
//         columnSizes = Vector(3, 2, 2, 2, 2))))
//     }
//   }

