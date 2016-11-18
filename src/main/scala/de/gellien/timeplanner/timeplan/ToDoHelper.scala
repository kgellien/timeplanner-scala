package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer

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
    val datespan = """(\d\d?)\.((\d\d?)\.)?(\d\d\d\d)?( - (\d\d?)\.(\d\d?)\.(\d\d\d\d)?)?""".r
    val datespan(fromDay, _, fromMonth, fromYear, to, toDay, toMonth, toYear) = dateInfo
    (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear)
  }

  def toIsoDate(day: String, month: String, year: String, pe: PeriodEntry) = {
    if (day == null) None
    else {
      val dd = day.toInt
      val yyyy = if (year != null) year.toInt else pe.year
      val MM =
        if (month != null)
          month.toInt
        else if (pe.lower.day <= dd || pe.upper.day < dd)
          pe.lower.month
        else
          pe.upper.month
      Some(IsoDate(yyyy, MM, dd))
    }
  }

  def extractTimeInfo(a: Appointment) = {
    a.periodEntry match {
      case pe: PeriodEntry =>
        if (!a.timeInfo.contains(":")) { // No Time; date only
          val (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = getDatespan(a.timeInfo)
          val fromIso = toIsoDate(fromDay, fromMonth, fromYear, pe)
          val toIso = toIsoDate(toDay, toMonth, toYear, pe) match {
            case to @ Some(isoDate) => to
            case None               => fromIso
          }
          (fromIso, toIso)
        } else {
          println(a)
          (None, None)
        }
      case pb: PeriodBase =>
        (None, None)
    }
  }

  def printTimeInfo(a: Appointment) = {
    val (fromIso, toIso) = extractTimeInfo(a)
    val prefix = a.periodEntry match {
      case pe: PeriodEntry => f"${pe} (${pe.lower} - ${pe.upper})"
      case pb: PeriodBase  => f"${pb}"
    }
    println(f"${prefix}) : ${a.timeInfo} : ${fromIso} - ${toIso}")
  }
}
