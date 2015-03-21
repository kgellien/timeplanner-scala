package de.gellien.timeplanner.timeplan

//import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.JavaTokenParsers

class ToDoListDsl extends JavaTokenParsers {

  def toDoItem = dateInfo ~ rep(timespan) ~ info ^^ { // TODO: check whether zero or once is possible with rep
    case d ~ t ~ i => ToDoEntry(d, t.mkString, i)
//    case d ~ t ~ i => val info = if (t.isEmpty) i else "\""+t.mkString+" "+i.substring(1) ; (d, info)
  }
  
  lazy val dateInfo = (day | week)
  
  lazy val day = (daily | calDay | weekDay)

  lazy val week = (weekly | calWeek)

  
  lazy val daily = "D" ^^ {case _ => DailyEntry()}
    
  lazy val calDay = """\d{4}-\d{2}-\d{2}""".r ^^ {case isoDate => DayEntry(isoDate)}
  
  
  lazy val weekDay = """[1..7]""".r ^? ({case i if ((1 to 7) contains i.toInt)  => WeekDayEntry(i.toInt)}, (i => "week day needs to be in the range of 1 to 7"))

  lazy val weekly = "W" ^^ {case _ => WeeklyEntry()}

  // TODO: use grouping instead of absolute position in String!
  lazy val calWeek = """\d{4}-W\d{2}""".r ^? ({case w if ((1 to 53) contains w.substring(7).toInt)  => WeekEntry(w)}, (i => "calendar week needs to be in the range of 1 to 53"))


//  lazy val timespan = """\d{2}:\d{2}""".r ^^ {case t => t.toString}
  lazy val timespan = """\d{2}:\d{2}( --? \d{2}:\d{2})?""".r ^^ {case t => t.toString}
  
  lazy val info = stringLiteral
  // the following alternatives do not work as expected!?!
//  lazy val info = """\.+""".r  //stringLiteral
//  lazy val info = "\\.*".r  //stringLiteral
  
}


case class ToDoEntry(val periodInfo: PeriodBase, val timeInfo: String, val info: String) {
  val strInfo = if (timeInfo.isEmpty) info else "\""+timeInfo+" "+info.substring(1)
  override def toString = "("+periodInfo + "," + strInfo+")"
}


abstract class PeriodBase

abstract class WeekBase extends PeriodBase
case class WeeklyEntry() extends WeekBase
case class WeekEntry(calWeekInYear: String) extends WeekBase {
  override def toString = "WeekEntry(%s)" format calWeekInYear
}

abstract class DayBase extends PeriodBase
case class DailyEntry() extends DayBase
case class DayEntry(isoDate: String) extends DayBase {
  override def toString = "DayEntry(%s)" format isoDate
}
case class WeekDayEntry(weekDay: Int) extends DayBase {
  override def toString = "WeekDayEntry(%d)" format weekDay 
}
