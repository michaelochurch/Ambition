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
 
  case class MarkupSegment(text:String, colorOpt:Option[Color]) {
    def length = text.length
    def display = colorOpt match {
      case Some(color) => textColor(color) + text + Console.RESET
      case None        => text
    }
  }

  sealed trait Alignment
  case object Left extends Alignment
  case object Right extends Alignment
  case object Center extends Alignment
  case object Default extends Alignment

  def spaces(n:Int) = {
    if (n > 0)
      new String(Array.fill(n)(' '))
    else ""
  }

  case class MarkupCell(alignment:Alignment, segments:MarkupSegment*) {
    val length = segments.map(_.length).sum

    def text = segments.map(_.text) mkString

    def content = segments.map(_.display) mkString

    def displayLeft(size:Int) = {
      content + spaces(size - length)
    }

    def displayRight(size:Int) = {
      spaces(size - length) + content
    }

    def displayCenter(size:Int) = {
      val n = size - length
      spaces((n + 1) / 2) + content + content(n / 2)
    }

    private def looksNumeric(s:String) = {
      (s.length > 1) && (s(0) == '-' || s(0) == '+' || s(0).isDigit) && 
      (s.drop(1).forall(c => c.isDigit || c == '.'))
    }

    def display(size:Int) = {
      val content = segments.map(_.display).mkString

      alignment match {
        case Left    => displayLeft(size)
        case Right   => displayRight(size)
        case Center  => displayCenter(size)
        case Default => if (looksNumeric(text)) displayRight(size) else displayLeft(size)
      }
    }
  }
  
  sealed trait MarkupRow {
    def lengths:Seq[Int]
  }
  
  case class Bar(ch:Char, left:String, sep:String, right:String) extends MarkupRow {
    def lengths = Seq[Int]()
  }

  case class DataRow(cells:MarkupCell*) extends MarkupRow {
    def lengths = cells.map(_.length)
  }

  case class LiteralString(s:String) extends MarkupRow {
    def lengths = Seq[Int]()
  }

  case class Table(rows:MarkupRow*) {
    private def maxLenVectors(v1:Seq[Int], v2:Seq[Int]) = {
      val maxLen = v1.length max v2.length
      (0 until maxLen).map(i => (v1.lift(i), v2.lift(i)) match {
        case (Some(n1), Some(n2)) => n1 max n2
        case (Some(n1), None)     => n1
        case (None    , Some(n2)) => n2
        case _                    => 0
      })
    }

    def display(columnLengths:Seq[Int] = Seq(), left:String = " ", sep:String = " ", right:String = " ") = {
      val actualColumnLengths = rows.foldLeft(columnLengths)((v, row) => maxLenVectors(v, row.lengths))
      val builder = new StringBuilder
      for (row <- rows) {
        row match {
          case LiteralString(s) => builder.append(s)
          case Bar(char, left, sep, right) => {
            val s = actualColumnLengths.map(n => new String(Array.fill(n)(char))).mkString(left, sep, right)
            builder.append(s)
          }
          case DataRow(rows@_*) => {
            val s = {
              actualColumnLengths.zip(rows).map {
                case (len, cell) => 
                  cell.display(len)
                }.mkString(left, sep, right)
            }
            builder.append(s)
          }
        }
        builder.append("\n")
      }
      builder.toString
    }
  }

  def clearScreen() = {
    print("\033[2J\033[;H")
  }

  val colorsBySym = Map("K" -> Black(false), "K+" -> Black(true), "R" -> Red(false), "R+" -> Red(true),
                        "G" -> Green(false), "G+" -> Green(true), "Y" -> Yellow(false), "Y+" -> Yellow(true),
                        "B" -> Blue(false), "B+" -> Blue(true), "M" -> Magenta(false), "M+" -> Magenta(true),
                        "C" -> Cyan(false), "C+" -> Cyan(true), "W" -> White(false), "W+" -> White(true))

  implicit def seg(s:String):MarkupSegment = MarkupSegment(s, None)

  def seg(s:String, colorInt:Int):MarkupSegment = MarkupSegment(s, Some(colorById(colorInt)))
  
  def seg(s:String, colorSym:String):MarkupSegment = {
    colorsBySym.get(colorSym) match {
      case Some(color) => MarkupSegment(s, Some(color))
      case None        => throw new Exception("bad color string: " + colorSym)
    }
  }

  def seg(s:String, color:Color):MarkupSegment = MarkupSegment(s, Some(color))

  def lCell(segs:MarkupSegment*) = MarkupCell(Left, segs:_*)

  def rCell(segs:MarkupSegment*) = MarkupCell(Right, segs:_*)

  def cCell(segs:MarkupSegment*) = MarkupCell(Center, segs:_*)

  def cell(segs:MarkupSegment*) = MarkupCell(Default, segs:_*)

  def main(args:Array[String]) = {
    clearScreen()
    for (i <- 0 to 15) {
      println(seg("%02d".format(i), i).display)
    }
    val t = Table(DataRow(cell("cats"), cell("dogs")),
                  DataRow(cell(seg("22", "R+")), cell(seg("45", "B"))))
    println(t.display())
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

