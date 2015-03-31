package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[String], val tasks: List[String])

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
    ToDoList(anniversaries.toList, appointments.toList.map { _.toLatex }, tasks.toList.map { _.toLatex })
  }

  def getTodoByDay(todos: List[ToDoEntry], currentDay: LocalDate, removeHeaderPrefix: Boolean): ToDoList = {
    val (month, day, year) = (currentDay.getMonthOfYear, currentDay.getDayOfMonth, currentDay.getYear)
    val pi = DayEntry(year, month, day)
    val pii = DailyEntry()
    val piii = WeekDayEntry(currentDay.getDayOfWeek)
    val piv = AnniversaryEntry(month, day)
    val tl = extract(todos, pi, pii, piii, piv)
    //    println("%s ---" format currentDay)
    //    println("  D Anniversaries new: %s" format tl.anniversaries.map { _.toLatex })
    //    println("  D Appointments new: %s" format tl.appointments)
    //    println("  D Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByWeek(todos: List[ToDoEntry], year: Int, week: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val pi = WeekEntry(year, week)
    val pii = WeeklyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByWeek ---")
    //    println("  W Appointments old: %s" format appointments)
    //    println("  W Appointments new: %s" format tl.appointments)
    //    println("  W Tasks old: %s" format tasks)
    //    println("  W Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByMonth(todos: List[ToDoEntry], year: Int, month: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val pi = MonthEntry(year, month)
    val pii = MonthlyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByMonth ---")
    //    println("  M Appointments new: %s" format tl.appointments)
    //    println("  M Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByQuarter(todos: List[ToDoEntry], year: Int, quarter: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val pi = QuarterEntry(year, quarter)
    val pii = QuarterlyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByQuarter ---")
    //    println("  Q Appointments new: %s" format tl.appointments)
    //    println("  Q Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByYear(todos: List[ToDoEntry], year: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val pi = YearEntry(year)
    val pii = YearlyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByYear ---")
    //    println("  Y Appointments new: %s" format tl.appointments)
    //    println("  Y Tasks new: %s" format tl.tasks)
    tl
  }
}
