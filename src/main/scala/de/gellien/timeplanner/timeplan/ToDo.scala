package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

abstract sealed class ToDo

case class Anniversary(val periodEntry: PeriodEntry, val yearOpt: Option[Int], val info: String) extends ToDo {
  override def toString = "Anniversary(%s, %s, %s)" format (periodEntry, yearOpt, info)
  def toLatex = yearOpt match {
    case Some(year) => "%s (%s)" format (info, year)
    case _          => info
  }
}

case class Appointment(val periodEntry: PeriodEntry, val classifierOpt: Option[String], val timeInfo: String, val info: String) extends ToDo {
  override def toString = "Appointment(%s, %s, %s, %s)" format (periodEntry, classifierOpt, timeInfo, info)
  def toLatex = "%s %s" format (timeInfo, info)
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s %s" format (classifier, timeInfo, info)
    case _                => "%s %s" format (timeInfo, info)
  }
}

case class Task(val periodEntry: PeriodEntry, val classifierOpt: Option[String], val dateBounds: List[DateBound], val info: String) extends ToDo {
  override def toString = "Task(%s, %s, %s, %s)" format (periodEntry, classifierOpt, dateBounds, info)
  def toLatex = info
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s" format (classifier, info)
    case _                => info
  }
}

object ToDoHelper {
  def extractTodosForPeriod(pe: PeriodEntry, todos: List[ToDo], pes: PeriodEntry*): ToDoList = {
    val anniversaries = new ListBuffer[Anniversary]
    val appointments = new ListBuffer[Appointment]
    val tasks = new ListBuffer[Task]
    for (todo <- todos; pb <- pe::(pes.toList)) {
      todo match {
        case Anniversary(`pb`, y, i)     => anniversaries += todo.asInstanceOf[Anniversary]
        case Appointment(`pb`, c, tp, i) => appointments += todo.asInstanceOf[Appointment]
        case Task(`pb`, c, Nil, i)       => tasks += todo.asInstanceOf[Task]
        case Task(`pb`, c, lst, i)       => if (pe.withinBounds(lst)) tasks += todo.asInstanceOf[Task]
        case _                           => ;
      }
    }
    ToDoList(anniversaries.toList, appointments.toList, tasks.toList)
  }

}
