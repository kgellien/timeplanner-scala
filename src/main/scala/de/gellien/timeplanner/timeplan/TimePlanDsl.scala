package de.gellien.timeplanner.timeplan

import scala.util.parsing.combinator.JavaTokenParsers

class TimePlanDsl(daysPerWeekPar: Int) extends JavaTokenParsers {
  implicit val daysPerWeek = daysPerWeekPar
  lazy val timeplan = (validTimeplan | ignore)

  lazy val validTimeplan = (weekLine | monthLine | quarterLine | yearLine)

  lazy val ignore = """.*""".r ^^ { case _ => None }

  lazy val weekLine = year ~ "-W" ~ weekNo ^^ { case year ~ str ~ weekNo => Some(WeekTimePlan(year, weekNo)) }

  lazy val monthLine = year ~ "-" ~ monthNo ^^ { case year ~ str ~ monthNo => Some(MonthTimePlan(year, monthNo)) }

  lazy val quarterLine = year ~ "-Q" ~ quarterNo ^^ { case year ~ str ~ quarterNo => Some(QuarterTimePlan(year, quarterNo)) }

  lazy val yearLine = year ^^ { case year => Some(YearTimePlan(year)) }

  lazy val year = """\d{4}""".r ^^ { case year => year.toInt }

  lazy val weekNo = """\d\d?""".r ^? ({ case weekNo if ((1 to 53) contains weekNo.toInt) => weekNo.toInt }, (weekNo => "week needs to be in the range of 1 to 53"))

  lazy val monthNo = """\d\d?""".r ^? ({ case monthNo if ((1 to 12) contains monthNo.toInt) => monthNo.toInt }, (monthNo => "month needs to be in the range of 1 to 12"))

  lazy val quarterNo = """\d\d?""".r ^? ({ case quarterNo if ((1 to 4) contains quarterNo.toInt) => quarterNo.toInt }, (quarterNo => "quarter needs to be in the range of 1 to 4"))
}

object TimePlanDsl {
  def getTimePlan(line: String, daysPerWeek: Int): Option[TimePlan] = {
    val tpd = new TimePlanDsl(daysPerWeek)
    val result = tpd.parseAll(tpd.timeplan, line) match {
      case tpd.Success(periodPlan, _) => periodPlan
      case tpd.Failure(msg, _) =>
        println("Problem with >%s<" format line)
        println("Failure: %s" format msg)
        None
      case tpd.Error(msg, _) =>
        println("Problem with >%s<" format line)
        println("Error: %s" format msg)
        None
    }
    result
  }
}
