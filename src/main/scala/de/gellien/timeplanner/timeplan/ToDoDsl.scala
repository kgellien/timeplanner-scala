package de.gellien.timeplanner.timeplan

import scala.util.parsing.combinator.JavaTokenParsers

class ToDoDsl extends JavaTokenParsers {

  lazy val toDoItem = (special | anniversary | appointment | task)

  lazy val special = "X" ~ calDate ~ info ^^ {
  	case _ ~ d ~ i => Special(d, i)
  }

  // TODO: check whether zero or once is possible with rep
  lazy val anniversary = anniversaryEntry ~ rep(yearNo) ~ info ^^ {
    case a ~ Nil ~ i => Anniversary(a, None, i)
    case a ~ y ~ i   => Anniversary(a, Some(y.head), i)
  }

  lazy val appointment = dateInfo ~ rep(classifier) ~ rep(dateBound) ~ timespan ~ info ^^ {
    case d ~ c ~ db ~ t ~ i => c match {
      case Nil => Appointment(d, None, db, t, i)
      case _   => Appointment(d, Some(c.head), db, t, i)
    }
  }

  lazy val task = dateInfo ~ rep(classifier) ~ rep(dateBound) ~ info ^^ {
    case d ~ c ~ db ~ i => c match {
      case Nil => Task(d, None, db, i)
      case _   => Task(d, Some(c.head), db, i)
    }
  }

  lazy val dateInfo = (weekDay | daily | weekly | monthly | quarterly | yearly | calDate)

  lazy val calDate = (calDay | calWeek | calMonth | calQuarter | calYear)
//  lazy val calDate = (calDayX | calDay | calWeek | calMonth | calQuarter | calYear)

//  lazy val calDayX = yearNo ~ "-" ~ monthNo ~ "-" ~ dayNo ~ "X" ^^ { case yearNo ~ s1 ~ monthNo ~ s2 ~ dayNo => DayEntry(yearNo, monthNo, dayNo) }

  lazy val calDay = yearNo ~ "-" ~ monthNo ~ "-" ~ dayNo ^^ { case yearNo ~ s1 ~ monthNo ~ s2 ~ dayNo => DayEntry(yearNo, monthNo, dayNo) }
  lazy val calWeek = yearNo ~ "-W" ~ weekNo ^^ { case year ~ str ~ weekNo => WeekEntry(year, weekNo) }
  lazy val calMonth = yearNo ~ "-" ~ monthNo ^^ { case year ~ str ~ monthNo => MonthEntry(year, monthNo) }
  lazy val calQuarter = yearNo ~ "-Q" ~ quarterNo ^^ { case year ~ str ~ quarterNo => QuarterEntry(year, quarterNo) }
  lazy val calYear = yearNo ^^ { case year => YearEntry(year) }

  lazy val anniversaryEntry = monthNo ~ "-" ~ dayNo ^^ { case monthNo ~ str ~ dayNo => AnniversaryEntry(monthNo, dayNo) }

  lazy val weekDay = """D\d""".r ^? ({ case i if ((1 to 7) contains i.tail.toInt) => WeekDayEntry(i.tail.toInt) }, (i => "week day needs to be in the range of 1 to 7"))

  lazy val daily = "D" ^^ { case _ => DailyEntry() }
  lazy val weekly = "W" ^^ { case _ => WeeklyEntry() }
  lazy val monthly = "M" ^^ { case _ => MonthlyEntry() }
  lazy val quarterly = "Q" ^^ { case _ => QuarterlyEntry() }
  lazy val yearly = "Y" ^^ { case _ => YearlyEntry() }

  lazy val timespan = (clockspan | datespan)

  lazy val clockspan = hhmm ~ opt("-") ~ opt(hhmm) ^^ {
    case from ~ Some(str) ~ Some(to) => Range(from, to)
    case from ~ str ~ None           => Range(from, from)
  }
  lazy val hhmm = hours ~ ":" ~ minutes ^^ { case hh ~ str ~ mm => Time(hh, mm) }
  lazy val minutes = """\d\d?""".r ^? ({ case minutes if ((0 to 59) contains minutes.toInt) => minutes.toInt }, (minutes => "day needs to be in the range of 0 to 59"))
  lazy val hours = """\d\d?""".r ^? ({ case hours if ((0 to 23) contains hours.toInt) => hours.toInt }, (hours => "hours needs to be in the range of 0 to 23"))

  lazy val datespan = ddmmyyyy ~ opt("-") ~ opt(ddmmyyyy) ^^ {
    case from ~ Some(str) ~ Some(to) => Range(from, to)
    case from ~ str ~ None           => Range(from, from)
  }
  lazy val ddmmyyyy = day ~ opt(month) ~ opt(yearNo) ^^ {
    case day ~ Some(month) ~ Some(year) => Date(year, month, day)
    case day ~ Some(month) ~ None       => Date(0, month, day)
    case day ~ None ~ None              => Date(0, 0, day)
  }

  lazy val dayMonth = day ~ month ^^ { case d ~ m => DayMonth(d, m) }
  lazy val day = dayNo ~ "." ^^ { case d ~ s => d }
  lazy val month = monthNo ~ "." ^^ { case m ~ s => m }

  lazy val classifier = "[" ~> text <~ "]" //^^ { case str => Some(str) }
  lazy val text = """\w*""".r

  lazy val info = """.*""".r

  lazy val dayNo = """\d\d?""".r ^? ({ case dayNo if ((1 to 31) contains dayNo.toInt) => dayNo.toInt }, (dayNo => "day needs to be in the range of 1 to 31"))
  lazy val weekNo = """\d\d?""".r ^? ({ case weekNo if ((1 to 53) contains weekNo.toInt) => weekNo.toInt }, (weekNo => "week needs to be in the range of 1 to 53"))
  lazy val monthNo = """\d\d?""".r ^? ({ case monthNo if ((1 to 12) contains monthNo.toInt) => monthNo.toInt }, (monthNo => "month needs to be in the range of 1 to 12"))
  lazy val quarterNo = """\d\d?""".r ^? ({ case quarterNo if ((1 to 4) contains quarterNo.toInt) => quarterNo.toInt }, (quarterNo => "quarter needs to be in the range of 1 to 4"))
  lazy val yearNo = """\d{4}""".r ^^ { case year => year.toInt }

  lazy val dateBound = (eqBound | neBound | ltBound | gtBound | leBound | geBound)

  lazy val eqBound = "=" ~> calDate ^^ { case pe => EqBound(pe) }
  lazy val neBound = "!" ~> calDate ^^ { case pe => NeBound(pe) }
  lazy val ltBound = "<" ~> calDate ^^ { case pe => LtBound(pe) }
  lazy val gtBound = ">" ~> calDate ^^ { case pe => GtBound(pe) }
  lazy val leBound = "<=" ~> calDate ^^ { case pe => LeBound(pe) }
  lazy val geBound = ">=" ~> calDate ^^ { case pe => GeBound(pe) }
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

  def getPeriodEntry(line: String): Option[PeriodEntry] = {
    val tdld = new ToDoDsl()
    val result = tdld.parseAll(tdld.calDate, line) match {
      case tdld.Success(pe, _) => Some(pe)
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
