package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

object ToDoHelper {
  def extract(todos: List[ToDoEntry], pbs: PeriodBase*): ToDoList = {
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
    val tl = extract(todos, DayEntry(year, month, day), DailyEntry(), WeekDayEntry(currentDay.getDayOfWeek), AnniversaryEntry(month, day))
    //    println("%s ---" format currentDay)
    //    println("  D Anniversaries new: %s" format tl.anniversaries.map { _.toLatex })
    //    println("  D Appointments new: %s" format tl.appointments.map { _.toLatex })
    //    println("  D Tasks new: %s" format tl.tasks.map { _.toLatex })
    tl
  }

  def getTodoByWeek(todos: List[ToDoEntry], year: Int, week: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val tl = extract(todos, WeekEntry(year, week), WeeklyEntry())
    //    println("getTodoByWeek ---")
    //    println("  W Appointments old: %s" format appointments)
    //    println("  W Appointments new: %s" format tl.appointments)
    //    println("  W Tasks old: %s" format tasks)
    //    println("  W Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByMonth(todos: List[ToDoEntry], year: Int, month: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val tl = extract(todos, MonthEntry(year, month), MonthlyEntry())
    //    println("getTodoByMonth ---")
    //    println("  M Appointments new: %s" format tl.appointments.map { _.toLatex })
    //    println("  M Tasks new: %s" format tl.tasks.map { _.toLatex })
    tl
  }

  def getTodoByQuarter(todos: List[ToDoEntry], year: Int, quarter: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val tl = extract(todos, QuarterEntry(year, quarter), QuarterlyEntry())
    //    println("getTodoByQuarter ---")
    //    println("  Q Appointments new: %s" format tl.appointments.map { _.toLatex })
    //    println("  Q Tasks new: %s" format tl.tasks.map { _.toLatex })
    tl
  }

  def getTodoByYear(todos: List[ToDoEntry], year: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val tl = extract(todos, YearEntry(year), YearlyEntry())
    //    println("getTodoByYear ---")
    //    println("  Y Appointments new: %s" format tl.appointments.map { _.toLatex })
    //    println("  Y Tasks new: %s" format tl.tasks.map { _.toLatex })
    tl
  }
}
