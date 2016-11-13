package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

abstract sealed class ToDo(val periodEntry: PeriodBase)

case class Anniversary(
    override val periodEntry: PeriodBase,
    val yearOpt: Option[Int],
    val info: String) extends ToDo(periodEntry) {
  def toLatex = yearOpt match {
    case Some(year) => "%s (%s)" format (info, year)
    case _          => info
  }
}

case class Appointment(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    val timeInfo: String,
    val info: String) extends ToDo(periodEntry) {
  val timeInfoForLatex = timeInfo.replace(" - ", " -- ")
  def toLatex = "%s %s" format (timeInfoForLatex, info)
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s" format (classifier, toLatex)
    case _                => toLatex
  }
  def extractSubTaskPeriodEntry(pe: PeriodEntry): List[Task] = {
    // periodEntry here is known to be of type PeriodEntry
    val first = TimeHelper.getFirstDayOfPeriod(pe)
    val last = TimeHelper.getLastDayOfPeriod(pe)
    val (fromDay, fromMonth, toDay, toMonth) = try {
      val re = """(\d\d?)\.(\d\d?)?\.? --? (\d\d?)?\.?(\d\d?)?\.?""".r
      val re(fds, fms, tds, tms) = timeInfo
      val fd = if (fds == null) None else Some(fds.toInt)
      val fm = if (fms == null) None else Some(fms.toInt)
      val td = if (tds == null) None else Some(tds.toInt)
      val tm = if (tms == null) None else Some(tms.toInt)
      (fd, fm, td, tm)
    } catch {
      case me: scala.MatchError => {
        println("ERROR in extractSubTasks for %s" format this)
        (None, None, None, None)
      }
    }
    val result = periodEntry match {
      case pe @ WeekEntry(y, w) => {
        // TODO: check questionable logic! Just good enough for *current* uses
        val monthFirst = first.getMonthOfYear
        val yearFirst = first.getYear
        val monthLast = last.getMonthOfYear
        val yearLast = last.getYear
        val fromD = fromDay.getOrElse(0)
        val toD = toDay.getOrElse(-1)
        val fromM = fromMonth.getOrElse(monthFirst)
        val toM = toMonth.getOrElse(monthLast)
        val from = DayEntry(yearFirst, fromM, fromD)
        val to = DayEntry(yearLast, toM, toD)
        val lst = for {
          day <- TimeHelper.daysInPeriod(from, to)
        } yield Task(day, classifierOpt, dateBounds, info)
        lst
      }
      case _ => Nil
    }
    result
  }
  def extractSubTasks(): List[Task] = {
    periodEntry match {
      case pe: PeriodEntry => extractSubTaskPeriodEntry(pe)
      case _               => Nil
    }
  }
}

case class Task(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    val info: String) extends ToDo(periodEntry) {
  def toLatex = info
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s" format (classifier, info)
    case _                => info
  }
}

object ToDoHelper {
  def extractTodosForPeriod(pe: PeriodEntry, todos: List[ToDo], pes: PeriodBase*): ToDoList = {
    val anniversaries = new ListBuffer[Anniversary]
    val appointments = new ListBuffer[Appointment]
    val tasks = new ListBuffer[Task]
    // TODO: try groupBy and filter
    for (todo <- todos; pb <- pe :: (pes.toList)) {
      todo match {
        case td @ Anniversary(`pb`, y, i)          => anniversaries += td
        case td @ Appointment(`pb`, c, Nil, tp, i) => appointments += td
        case td @ Appointment(`pb`, c, lst, tp, i) => if (pe.withinBounds(lst)) appointments += td
        case td @ Task(`pb`, c, Nil, i)            => tasks += td
        case td @ Task(`pb`, c, lst, i)            => if (pe.withinBounds(lst)) tasks += td
        case _                                     => ;
      }
    }
    ToDoList(anniversaries.toList, appointments.toList, tasks.toList)
  }

  def getDatespan(dateInfo: String) = {
    //    val datespan = """(\d\d?)\.(\d\d?)?\.?(\d\d\d\d)?( --? (\d\d?)?\.?(\d\d?)?\.?(\d\d\d\d)?)?""".r
    val datespan = """(\d\d?)\.(\d\d?)?\.?(\d\d\d\d)?( --? (\d\d?)\.(\d\d?)\.(\d\d\d\d)?)?""".r
    val datespan(fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = dateInfo
    (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear)
  }

  def toIsoDate(day: String, month: String, year: String, pe: PeriodEntry) = {
    pe match {
      case we: WeekEntry =>
        val yyyy = if (year != null) year.toInt else pe.year
        val MM = if (month != null) month.toInt else if (pe.lower.day <= day.toInt || pe.upper.day < day.toInt) pe.lower.month else pe.upper.month
        val dd = day.toInt
        IsoDate(yyyy, MM, dd)
      case _ =>
        val yyyy = if (year != null) year.toInt else pe.year
        val MM = if (month != null) month.toInt else if (pe.lower.day <= day.toInt || pe.upper.day < day.toInt) pe.lower.month else pe.upper.month
        val dd = day.toInt
        IsoDate(yyyy, MM, dd)
    }
  }

  def extractTimeInfo(a: Appointment) = {
    if (!a.timeInfo.contains(":")) { // No Time; date only
      val (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = getDatespan(a.timeInfo)
      //      println(fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear)
      val fromIso = toIsoDate(fromDay, fromMonth, fromYear, a.periodEntry.asInstanceOf[PeriodEntry])
      val toIso = try {
        toIsoDate(toDay, toMonth, toYear, a.periodEntry.asInstanceOf[PeriodEntry])
      } catch {
        case e: NumberFormatException => fromIso
      }
      //      val cols = a.timeInfo.split("-")
      a.periodEntry match {
        case pe: PeriodEntry =>
          (Some(fromIso), Some(toIso))
        case pb: PeriodBase =>
          (None, None)
      }
    } else (None, None)
  }

  def printTimeInfo(a: Appointment) = {
    if (!a.timeInfo.contains(":")) { // No Time; date only
      val (fromIso, toIso) = extractTimeInfo(a)
      val prefix = a.periodEntry match {
        case pe: PeriodEntry =>
          f"${pe} (${pe.lower} - ${pe.upper})"
        case pb: PeriodBase =>
          f"${pb}"
      }
      println(f"${prefix}) : ${a.timeInfo} : ${fromIso} - ${toIso}")
    }
  }
}
