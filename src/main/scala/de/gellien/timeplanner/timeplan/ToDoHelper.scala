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

  def toIsoDate(date: Date, pe: PeriodEntry) = {
    if (date.day == 0) None
    else {
      val dd = date.day
      val yyyy = if (date.year != 0) date.year else pe.year
      val MM =
        if (date.month != 0)
          date.month
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
        a.timeInfo match {
          case Range(from @ Date(fromYear, fromMonth, fromDay), to @ Date(toYear, toMonth, toDay)) =>
            val fromIso = toIsoDate(from, pe)
            val toIso = toIsoDate(from, pe)
            (fromIso, toIso)
          case _ => (None, None)
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
    println(f"TimeInfo: ${prefix}) : ${a.timeInfo} : ${fromIso} - ${toIso}")
  }
}
