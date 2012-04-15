package de.gellien.timeplanner.timeplan

class BoundChecker(task: String, dateFilterStr: String) {
  val contents = task.split(" ").toList
  val dateFilterEither = BoundChecker.getDateFilter(dateFilterStr)
  def restricted(lst: List[String]) = List('>', '<', '!', '=') contains lst.head.charAt(0)
  def checkFirstRestriction(lst: List[String]): Option[List[String]] = {
    if (restricted(lst)) {
      val operatorStr = lst.head.charAt(0)
      val limitStr = lst.head.substring(1)
      val limitEither = BoundChecker.getDateFilter(limitStr)
      // works for day, month, year; week and quarter need special treatment!
      val result = operatorStr match { // Pre: tail contains at least one non restricted entry!
        case '<' => if (dateFilterStr < limitStr) checkFirstRestriction(lst.tail) else None
        case '>' => if (dateFilterStr > limitStr) checkFirstRestriction(lst.tail) else None
        case '!' => if (dateFilterStr != limitStr) checkFirstRestriction(lst.tail) else None
        case '=' =>
          val minLength = if (dateFilterStr.length > limitStr.length) limitStr.length else dateFilterStr.length
          val df = dateFilterStr.substring(0, minLength)
          val li = limitStr.substring(0, minLength)
          if (df == li) checkFirstRestriction(lst.tail) else None
      }
      result
    } else Some(lst)
  }
  val result = checkFirstRestriction(contents) match {
    case None => None
    case Some(lst) => Some(lst mkString " ")
  }
}

object BoundChecker {
  def getDateFilter(dateFilterStr: String) = {
    val dfd = new DateFilterDsl
    dfd.parseAll(dfd.dateFilter, dateFilterStr) match {
      case dfd.Success(dateFilter, _) => Right(dateFilter)
      case dfd.Failure(msg, _) => Left("Failure: " + msg)
      case dfd.Error(msg, _) => Left("Error: " + msg)
    }
  }
}

/////////////////////
// for future use (still experimental)

sealed abstract class DateFilter

case class DayFilter(yearMonthDay: String) extends DateFilter
case class WeekFilter(yearWeek: String) extends DateFilter
case class MonthFilter(yearMonth: String) extends DateFilter
case class QuarterFilter(yearQuarter: String) extends DateFilter
case class YearFilter(year: String) extends DateFilter

object DateFilter {
  // Fill with comparison functions
}

import scala.util.parsing.combinator.JavaTokenParsers

class DateFilterDsl extends JavaTokenParsers {
  lazy val dateFilter = (dayFilter | weekFilter | monthFilter | quarterFilter | yearFilter)
  lazy val dayFilter = """\d{4}-\d{2}-\d{2}""".r ^^ {case yearMonthDay => DayFilter(yearMonthDay)}
  lazy val weekFilter = """\d{4}-KW-\d{2}""".r ^^ {case yearWeek => WeekFilter(yearWeek)}
  lazy val monthFilter = """\d{4}-\d{2}""".r ^^ {case yearMonth => MonthFilter(yearMonth)}
  lazy val quarterFilter = """\d{4}-Q\d""".r ^^ {case yearQuarter => QuarterFilter(yearQuarter)}
  lazy val yearFilter = """\d{4}""".r ^^ {case year => YearFilter(year)}
// TODO: do it with plausibility tests !?!
//  lazy val quarterNo = """\d""".r ^? ({case quarterNo if ((1 to 4) contains quarterNo.toInt) => quarterNo.toInt}, (quarterNo => "quarter needs to be in the range of 1 to 4"))
}
