package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

abstract sealed class ToDo

case class Anniversary(val periodEntry: PeriodBase, val yearOpt: Option[Int], val info: String) extends ToDo {
  def toLatex = yearOpt match {
    case Some(year) => "%s (%s)" format (info, year)
    case _          => info
  }
}

case class Appointment(val periodEntry: PeriodBase, val classifierOpt: Option[String], val dateBounds: List[DateBound], val timeInfo: String, val info: String) extends ToDo {
  def toLatex = "%s %s" format (timeInfo, info)
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s %s" format (classifier, timeInfo, info)
    case _                => "%s %s" format (timeInfo, info)
  }
}

case class Task(val periodEntry: PeriodBase, val classifierOpt: Option[String], val dateBounds: List[DateBound], val info: String) extends ToDo {
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
}
