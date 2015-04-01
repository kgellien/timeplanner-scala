package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

abstract sealed class ToDoEntry

case class Anniversary(val periodEntry: PeriodEntry, val yearOpt: Option[Int], val info: String) extends ToDoEntry {
  override def toString = "Anniversary(%s, %s, %s)" format (periodEntry, yearOpt, info)
  def toLatex = yearOpt match {
    case Some(year) => "%s (%s)" format (info, year)
    case _ => info
  }
}

case class Appointment(val periodEntry: PeriodEntry, val classifierOpt: Option[String], val timeInfo: String, val info: String) extends ToDoEntry {
  override def toString = "Appointment(%s, %s, %s, %s)" format (periodEntry, classifierOpt, timeInfo, info)
  def toLatex = "%s %s" format (timeInfo, info)
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s %s" format (classifier, timeInfo, info)
    case _ => "%s %s" format (timeInfo, info)
  }
}

case class Task(val periodEntry: PeriodEntry, val classifierOpt: Option[String], val info: String) extends ToDoEntry {
  override def toString = "Task(%s, %s, %s)" format (periodEntry, classifierOpt, info)
  def toLatex = info
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s" format (classifier, info)
    case _ => info
  }
}


object ToDoHelper {
  def extract(todos: List[ToDoEntry], pbs: PeriodEntry*): ToDoList = {
    val anniversaries = new ListBuffer[Anniversary]
    val appointments = new ListBuffer[Appointment]
    val tasks = new ListBuffer[Task]
    for (todo <- todos; pb <- pbs) {
      todo match {
        case Anniversary(`pb`, y, i)     => anniversaries += todo.asInstanceOf[Anniversary]
        case Appointment(`pb`, c, tp, i) => appointments += todo.asInstanceOf[Appointment]
        case Task(`pb`, c, i)            => tasks += todo.asInstanceOf[Task]
        case _                           => ;
      }
    }
    ToDoList(anniversaries.toList, appointments.toList, tasks.toList)
  }

  def getTodoByDay(todos: List[ToDoEntry], currentDay: LocalDate, removeHeaderPrefix: Boolean): ToDoList = {
    val (month, day, year) = (currentDay.getMonthOfYear, currentDay.getDayOfMonth, currentDay.getYear)
    extract(todos, DayEntry(year, month, day), DailyEntry(), WeekDayEntry(currentDay.getDayOfWeek), AnniversaryEntry(month, day))
  }

  def getTodoByWeek(todos: List[ToDoEntry], year: Int, week: Int, removeHeaderPrefix: Boolean): ToDoList = {
    extract(todos, WeekEntry(year, week), WeeklyEntry())
  }

  def getTodoByMonth(todos: List[ToDoEntry], year: Int, month: Int, removeHeaderPrefix: Boolean): ToDoList = {
    extract(todos, MonthEntry(year, month), MonthlyEntry())
  }

  def getTodoByQuarter(todos: List[ToDoEntry], year: Int, quarter: Int, removeHeaderPrefix: Boolean): ToDoList = {
    extract(todos, QuarterEntry(year, quarter), QuarterlyEntry())
  }

  def getTodoByYear(todos: List[ToDoEntry], year: Int, removeHeaderPrefix: Boolean): ToDoList = {
    extract(todos, YearEntry(year), YearlyEntry())
  }
}
