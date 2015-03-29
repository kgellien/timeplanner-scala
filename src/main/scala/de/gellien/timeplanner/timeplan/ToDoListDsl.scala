package de.gellien.timeplanner.timeplan

import scala.util.parsing.combinator.JavaTokenParsers

class ToDoListDsl extends JavaTokenParsers {

  lazy val toDoItem = (anniversary | appointmentOrTask)
  
  lazy val anniversary = monthNo ~ "-" ~ dayNo ~ rep(yearNo) ~ info ^^ {
    case m ~ str ~ d ~ Nil ~ i => Anniversary(m, d, None, i)
    case m ~ str ~ d ~ y ~ i => Anniversary(m, d, Some(y.head), i)
  }
  
  lazy val  appointmentOrTask = dateInfo ~ rep(classifier) ~ rep(timespan) ~ info ^^ { // TODO: check whether zero or once is possible with rep
    case d ~ c ~ Nil ~ i => c match {
      case Nil => Task(d, None, i)
      case _ => Task(d, Some(c.head), i)
    }
    case d ~ c ~ t ~ i => c match {
      case Nil => Appointment(d, None, t.mkString, i)
      case _ => Appointment(d, Some(c.head), t.mkString, i)
    }
  }

  lazy val dateInfo = (day | week | month | quarter | year)

  lazy val day = (weekDay | daily | calDay)
  lazy val week = (weekly | calWeek)
  lazy val month = (monthly | calMonth)
  lazy val quarter = (quarterly | calQuarter)
  lazy val year = (yearly | calYear)
  
  lazy val timespan = (clockspan | datespan)

  lazy val daily = "D" ^^ { case _ => DailyEntry() }
  lazy val calDay = yearNo ~ "-" ~ monthNo ~ "-" ~ dayNo ^^ { case yearNo ~ s1 ~ monthNo ~ s2~ dayNo => DayEntry(yearNo, monthNo, dayNo) }
  lazy val weekDay = """D\d""".r ^? ({ case i if ((1 to 7) contains i.tail.toInt) => WeekDayEntry(i.tail.toInt) }, (i => "week day needs to be in the range of 1 to 7"))

  lazy val weekly = "W" ^^ { case _ => WeeklyEntry() }
  lazy val calWeek = yearNo ~ "-W" ~ weekNo ^^ { case year ~ str ~ weekNo => WeekEntry(year, weekNo) }

  lazy val monthly = "M" ^^ { case _ => MonthlyEntry() }
  lazy val calMonth = yearNo ~ "-" ~ monthNo ^^ { case year ~ str ~ monthNo => MonthEntry(year, monthNo) }

  lazy val quarterly = "Q" ^^ { case _ => QuarterlyEntry() }
  lazy val calQuarter = yearNo ~ "-Q" ~ quarterNo ^^ { case year ~ str ~ quarterNo => QuarterEntry(year, quarterNo) }

  lazy val yearly = "Y" ^^ { case _ => YearlyEntry() }
  lazy val calYear = yearNo ^^ { case year => YearEntry(year) }

  lazy val clockspan = """\d\d?:\d{2}( --? \d\d?:\d{2})?""".r ^^ { case t => t.toString }
  lazy val datespan = """\d\d?\.\d?\d?\.?( --? \d\d?.\d\d?\.)?""".r ^^ { case t => t.toString }

  lazy val classifier = "[" ~> text <~ "]" //^^ { case str => Some(str) }
  lazy val text = """\w*""".r

  lazy val info = """.*""".r

  lazy val dayNo = """\d\d?""".r ^? ({ case dayNo if ((1 to 31) contains dayNo.toInt) => dayNo.toInt }, (dayNo => "day needs to be in the range of 1 to 31"))

  lazy val weekNo = """\d\d?""".r ^? ({ case weekNo if ((1 to 53) contains weekNo.toInt) => weekNo.toInt }, (weekNo => "week needs to be in the range of 1 to 53"))

  lazy val monthNo = """\d\d?""".r ^? ({ case monthNo if ((1 to 12) contains monthNo.toInt) => monthNo.toInt }, (monthNo => "month needs to be in the range of 1 to 12"))

  lazy val quarterNo = """\d\d?""".r ^? ({ case quarterNo if ((1 to 4) contains quarterNo.toInt) => quarterNo.toInt }, (quarterNo => "quarter needs to be in the range of 1 to 4"))

  lazy val yearNo = """\d{4}""".r ^^ { case year => year.toInt }
}

object ToDoListDsl {
  def getToDo(line: String): Option[ToDoEntry] = {
    val tdld = new ToDoListDsl()
    val result = tdld.parseAll(tdld.toDoItem, line) match {
      case tdld.Success(toDoItem, _) => Some(toDoItem)
      case tdld.Failure(msg, _) =>
        println("Failure parsing line >%s<: %s" format (line, msg))
        None
      case tdld.Error(msg, _) =>
        println("Error parsing line >%s<: %s" format (line, msg))
        None
    }
    result
  }
}

abstract sealed class ToDoEntry
case class Anniversary(val month: Int, val day: Int, val year: Option[Int], val info: String) extends ToDoEntry {
  override def toString = "Anniversary(%d, %d, %s, %s)" format (month, day, year, info)
}

case class Appointment(val periodInfo: PeriodBase, val classifier: Option[String], val timeInfo: String, val info: String) extends ToDoEntry {
  override def toString = "Appointment(%s, %s, %s, %s)" format (periodInfo, classifier, timeInfo, info)
}

case class Task(val periodInfo: PeriodBase, val classifier: Option[String], val info: String) extends ToDoEntry {
  override def toString = "Task(%s, %s, %s)" format (periodInfo, classifier, info)
}

abstract sealed class PeriodBase

abstract sealed class YearBase extends PeriodBase
case class YearlyEntry() extends YearBase
case class YearEntry(year: Int) extends YearBase {
  override def toString = "YearEntry(%d)" format (year)
}

abstract sealed class QuarterBase extends PeriodBase
case class QuarterlyEntry() extends QuarterBase
case class QuarterEntry(year: Int, quarter: Int) extends QuarterBase {
  override def toString = "QuarterEntry(%d-Q%2d)" format (year, quarter)
}

abstract sealed class MonthBase extends PeriodBase
case class MonthlyEntry() extends MonthBase
case class MonthEntry(year: Int, month: Int) extends MonthBase {
  override def toString = "MonthEntry(%d-%02d)" format (year, month)
}

abstract sealed class WeekBase extends PeriodBase
case class WeeklyEntry() extends WeekBase
case class WeekEntry(year: Int, week: Int) extends WeekBase {
  override def toString = "WeekEntry(%d-W%02d)" format (year, week)
}

abstract sealed class DayBase extends PeriodBase
case class DailyEntry() extends DayBase
case class DayEntry(yearNo: Int, monthNo: Int, dayNo: Int) extends DayBase {
  override def toString = "DayEntry(%d-%02d-%02d)" format (yearNo, monthNo, dayNo)
}
case class WeekDayEntry(weekDay: Int) extends DayBase {
  override def toString = "WeekDayEntry(%d)" format weekDay
}
