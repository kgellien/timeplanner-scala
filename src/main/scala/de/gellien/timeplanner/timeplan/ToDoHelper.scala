package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer

object ToDoHelper {
  def extractTodosForPeriod(pe: PeriodEntry, todos: List[ToDo], pes: PeriodBase*): ToDoList = {
  	val specials = new ListBuffer[Special]
    val anniversaries = new ListBuffer[Anniversary]
    val appointments = new ListBuffer[Appointment]
    val tasks = new ListBuffer[Task]
    // TODO: try groupBy and filter
    for (todo <- todos; pb <- pe :: (pes.toList)) {
      todo match {
        case td @ Special(`pb`, i)                 => specials += td
        case td @ Anniversary(`pb`, y, i)          => anniversaries += td
        case td @ Appointment(`pb`, c, Nil, tp, i) => appointments += td
        case td @ Appointment(`pb`, c, lst, tp, i) => if (pe.withinBounds(lst)) appointments += td
        case td @ Task(`pb`, c, Nil, i)            => tasks += td
        case td @ Task(`pb`, c, lst, i)            => if (pe.withinBounds(lst)) tasks += td
        case _                                     => ;
      }
    }
    ToDoList(specials.toList, anniversaries.toList, appointments.toList, tasks.toList)
  }
}
