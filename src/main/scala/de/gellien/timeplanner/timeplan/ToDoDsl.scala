package de.gellien.timeplanner.timeplan

import scala.util.parsing.combinator.JavaTokenParsers

class ToDoDsl extends JavaTokenParsers {

  lazy val toDoItem = (anniversary | appointmentOrTask)
  
  lazy val anniversary = anniversaryEntry ~ rep(yearNo) ~ info ^^ {
    case a ~ Nil ~ i => Anniversary(a, None, i)
    case a ~ y ~ i => Anniversary(a, Some(y.head), i)
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

  lazy val dateInfo = (anniversaryEntry | day | week | month | quarter | year)

  lazy val day = (weekDay | daily | calDay)
  lazy val week = (weekly | calWeek)
  lazy val month = (monthly | calMonth)
  lazy val quarter = (quarterly | calQuarter)
  lazy val year = (yearly | calYear)
  
  lazy val timespan = (clockspan | datespan)

  lazy val anniversaryEntry = monthNo ~ "-" ~ dayNo ^^ { case monthNo ~ str ~ dayNo => AnniversaryEntry(monthNo, dayNo) }
  
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

object ToDoDsl {
  def getToDo(line: String): Option[ToDo] = {
    val tdld = new ToDoDsl()
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
