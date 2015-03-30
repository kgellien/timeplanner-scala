package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[String], val tasks: List[String])

object ToDoHelper {
  def getEntriesWithPrefix(lines: List[String], prefix: String) = {
    (for (line <- lines if line.startsWith(prefix)) yield line.replaceFirst(prefix, "")).sortWith((e1, e2) => (e1 compareTo e2) < 0)
  }

  val dailySearchPattern = "D "
  val weeklySearchPattern = "W "
  val monthlySearchPattern = "M "
  val quarterlySearchPattern = "Q "
  val yearlySearchPattern = "Y "
  val calendarWeekSearchPattern = "%d-W%02d "
  val calendarMonthSearchPattern = "%d-%02d "
  val calendarQuarterSearchPattern = "%d-Q%d "
  val calendarYearSearchPattern = "%d "

  def isAppointment(entry: String) = {
    val pos = if (PeriodSplitter.startsWithTaskHeaderPrefix(entry))
      PeriodSplitter.taskHeaderPrefixSize
    else
      0
    entry.charAt(pos).isDigit &&
      (entry.charAt(pos + 1).isDigit || (List('.', ':') contains entry.charAt(pos + 1)))
  }

  def lexicalOrdering = (e1: String, e2: String) => (e1 compareTo e2) < 0

  def filterWorkLists(workList: List[String], dateFilter: String, prefixes: String*): List[String] =
    for {
      task <- (for (prefix <- prefixes) yield getEntriesWithPrefix(workList, prefix)).flatten.toList
      result = new BoundChecker(task, dateFilter).result
      if (result != None)
    } yield result.get

  def appointmentsAndTasks(workList: List[String], dateFilter: String, removeHeaderPrefix: Boolean, prefixes: String*): (List[String], List[String]) = {
    def prepareEntry(entry: String, removeHeaderPrefix: Boolean) = // remove taskHeaderPrefix if necessary
      if (removeHeaderPrefix && PeriodSplitter.startsWithTaskHeaderPrefix(entry)) entry.substring(PeriodSplitter.taskHeaderPrefixSize)
      else entry
    val todo = filterWorkLists(workList, dateFilter, prefixes: _*)
    val appointments = (for (entry <- todo if isAppointment(entry)) yield prepareEntry(entry, removeHeaderPrefix)).sortWith(lexicalOrdering)
    val tasks = (for (entry <- todo if !isAppointment(entry)) yield prepareEntry(entry, removeHeaderPrefix)).sortWith(lexicalOrdering)
    (appointments, tasks)
  }

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

  def getTodoByDay(todos: List[ToDoEntry], workList: List[String], currentDay: LocalDate, removeHeaderPrefix: Boolean): ToDoList = {
    val prefixes = List(isoDate(currentDay) + " ", dailySearchPattern, currentDay.getDayOfWeek().toString + " ")
    val (appointments, tasks) = appointmentsAndTasks(workList, isoDate(currentDay), removeHeaderPrefix, prefixes: _*)
    //
    val (month, day, year) = (currentDay.getMonthOfYear, currentDay.getDayOfMonth, currentDay.getYear)
    val pi = DayEntry(year, month, day)
    val pii = DailyEntry()
    val piii = WeekDayEntry(currentDay.getDayOfWeek)
    val piv = AnniversaryEntry(month, day)
    val tl = extract(todos, pi, pii, piii, piv)
    //    println("%s ---" format currentDay)
    //    println("  D Anniversaries new: %s" format tl.anniversaries.map { _.toLatex })
    //    println("  D Appointments old: %s" format appointments)
    //    println("  D Appointments new: %s" format tl.appointments)
    //    println("  D Tasks old: %s" format tasks)
    //    println("  D Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByWeek(todos: List[ToDoEntry], workList: List[String], year: Int, week: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calWeek = calendarWeekSearchPattern.format(year, week).stripSuffix(" ")
    val prefixes = List(calendarWeekSearchPattern format (year, week), weeklySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calWeek, removeHeaderPrefix, prefixes: _*)
    val anniversaries = List()
    //
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

  def getTodoByMonth(todos: List[ToDoEntry], workList: List[String], year: Int, month: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calMonth = calendarMonthSearchPattern.format(year, month).stripSuffix(" ")
    val prefixes = List(calendarMonthSearchPattern format (year, month), monthlySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calMonth, removeHeaderPrefix, prefixes: _*)
    val anniversaries = List()
    //
    val pi = MonthEntry(year, month)
    val pii = MonthlyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByMonth ---")
    //    println("  M Appointments old: %s" format appointments)
    //    println("  M Appointments new: %s" format tl.appointments)
    //    println("  M Tasks old: %s" format tasks)
    //    println("  M Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByQuarter(todos: List[ToDoEntry], workList: List[String], year: Int, quarter: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calQuarter = calendarQuarterSearchPattern.format(year, quarter).stripSuffix(" ")
    val prefixes = List(calendarQuarterSearchPattern format (year, quarter), quarterlySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calQuarter, removeHeaderPrefix, prefixes: _*)
    val anniversaries = List()
    //
    val pi = QuarterEntry(year, quarter)
    val pii = QuarterlyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByQuarter ---")
    //    println("  Q Appointments old: %s" format appointments)
    //    println("  Q Appointments new: %s" format tl.appointments)
    //    println("  Q Tasks old: %s" format tasks)
    //    println("  Q Tasks new: %s" format tl.tasks)
    tl
  }

  def getTodoByYear(todos: List[ToDoEntry], workList: List[String], year: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calYear = calendarYearSearchPattern.format(year, year).stripSuffix(" ")
    val prefixes = List(calendarYearSearchPattern format (year), yearlySearchPattern)
    val (appointmentsOld, tasksOld) = appointmentsAndTasks(workList, calYear, removeHeaderPrefix, prefixes: _*)
    val anniversaries = List()
    //
    val pi = YearEntry(year)
    val pii = YearlyEntry()
    val tl = extract(todos, pi, pii)
    //    println("getTodoByYear ---")
    //    println("  Y Appointments old: %s" format appointments)
    //    println("  Y Appointments new: %s" format tl.appointments)
    //    println("  Y Tasks old: %s" format tasks)
    //    println("  Y Tasks new: %s" format tl.tasks)
    tl
  }
}
