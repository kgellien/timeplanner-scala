package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate
import TimeHelper._

abstract class PeriodPlan(val withOverview: Boolean) {
  val todo: List[ToDoList]
  val period: SinglePeriod
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
  val periodSpecifics: List[SinglePeriod]
}

case class WeekPlan(year: Int, week: Int, workList: List[String], override val withOverview: Boolean = true) extends PeriodPlan(withOverview) {
  override val todo = PeriodPlan.getAppointmentsByWeek(workList, year, week, removeHeaderPrefix = false)
  override val period = Week(year, week, todo)
  override val periodSpecifics = for (currentDay <- daysInWeek(year, week))
    yield Day(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth, PeriodPlan.getAppointmentsByDay(workList, currentDay, removeHeaderPrefix = true))
}

case class MonthPlan(year: Int, month: Int, workList: List[String], override val withOverview: Boolean = true) extends PeriodPlan(withOverview) {
  override val todo = PeriodPlan.getAppointmentsByMonth(workList, year, month, removeHeaderPrefix = false)
  override val period = Month(year, month, todo)
  override val periodSpecifics = for ((weekYear, week) <- weeksInMonth(year, month))
    yield Week(weekYear, week, PeriodPlan.getAppointmentsByWeek(workList, weekYear, week, removeHeaderPrefix = true))
}

case class QuarterPlan(year: Int, quarter: Int, workList: List[String], override val withOverview: Boolean = true) extends PeriodPlan(withOverview) {
  override val todo = PeriodPlan.getAppointmentsByQuarter(workList, year, quarter, removeHeaderPrefix = false)
  override val period = Quarter(year, quarter, todo)
  override val periodSpecifics = for (month <- monthsInQuarter(year, quarter))
    yield Month(year, month, PeriodPlan.getAppointmentsByMonth(workList, year, month, removeHeaderPrefix = true))
}

case class YearPlan(year: Int, workList: List[String], override val withOverview: Boolean = true) extends PeriodPlan(withOverview) {
  override val todo = PeriodPlan.getAppointmentsByYear(workList, year, removeHeaderPrefix = false)
  override val period = Year(year, todo)
  override val periodSpecifics = for (quarter <- (1 to 4).toList)
    yield Quarter(year, quarter, PeriodPlan.getAppointmentsByQuarter(workList, year, quarter, removeHeaderPrefix = true))
}

object PeriodPlan {
  def getEntriesWithPrefix(lines: List[String], prefix: String) = {
    (for (line <- lines if line.startsWith(prefix)) yield line.replaceFirst(prefix, "")).sortWith((e1, e2) => (e1 compareTo e2) < 0)
  }

  val dailySearchPattern = "D "
  val weeklySearchPattern = "W "
  val monthlySearchPattern = "M "
  val quarterlySearchPattern = "Q "
  val yearlySearchPattern = "Y "
  val calendarWeekSearchPattern = "%d-KW-%02d "
  val calendarMonthSearchPattern = "%d-%02d "
  val calendarQuarterSearchPattern = "%d-Q%d "
  val calendarYearSearchPattern = "%d "

  def isAppointment(entry: String) = {
    val pos = if (PeriodSplitter.startsWithTaskHeaderPrefix(entry))
      PeriodSplitter.taskHeaderPrefixSize
    else
      0
    entry.charAt(pos).isDigit &&
    (entry.charAt(pos+1).isDigit || (List('.', ':') contains entry.charAt(pos+1)))
  }
  
  def lexicalOrdering = (e1: String, e2: String) => (e1 compareTo e2) < 0
  
  def filterWorkLists(workList: List[String], dateFilter: String, prefixes: String*): List[String] = {
    for {
      task <- (for (prefix <- prefixes) yield getEntriesWithPrefix(workList, prefix)).flatten.toList
      result = new BoundChecker(task, dateFilter).result
      if (result != None)
    } yield result.get
  }
  
  def appointmentsAndTasks(workList: List[String], dateFilter: String, removeHeaderPrefix: Boolean, prefixes: String*): List[ToDoList] = {
    def prepareEntry(entry: String, removeHeaderPrefix: Boolean) = { // remove taskHeaderPrefix if necessary
      if (removeHeaderPrefix && PeriodSplitter.startsWithTaskHeaderPrefix(entry)) entry.substring(PeriodSplitter.taskHeaderPrefixSize)
      else entry
    }
    val todo = filterWorkLists(workList, dateFilter, prefixes:_*)
    val appointments = (for (entry <- todo if isAppointment(entry)) yield prepareEntry(entry, removeHeaderPrefix)).sortWith(lexicalOrdering)
    val tasks = (for (entry <- todo if !isAppointment(entry)) yield prepareEntry(entry, removeHeaderPrefix)).sortWith(lexicalOrdering)
    List(new ToDoList(Appointment, appointments), new ToDoList(Task, tasks))
  }

  // use List[List[String]], i.e. Helper.ToDoLists, to be able to differentiate in output
  def getAppointmentsByDay(workList: List[String], currentDay: LocalDate, removeHeaderPrefix: Boolean): List[ToDoList] = {
    val anniversaries = getEntriesWithPrefix(workList, isoDate(currentDay).substring(5)+" ")
    val prefixes = List(isoDate(currentDay)+" ", dailySearchPattern, currentDay.getDayOfWeek().toString+" ")
    new ToDoList(Anniversary, anniversaries) :: appointmentsAndTasks(workList, isoDate(currentDay), removeHeaderPrefix, prefixes: _*)
  }

  def getAppointmentsByWeek(workList: List[String], year: Int, week: Int, removeHeaderPrefix: Boolean): List[ToDoList] = {
    val calWeek = calendarWeekSearchPattern.format(year, week).stripSuffix(" ")
    val prefixes = List(calendarWeekSearchPattern format (year, week), weeklySearchPattern)
    appointmentsAndTasks(workList, calWeek, removeHeaderPrefix, prefixes: _*)
  }

  def getAppointmentsByMonth(workList: List[String], year: Int, month: Int, removeHeaderPrefix: Boolean): List[ToDoList] = {
    val calMonth = calendarMonthSearchPattern.format(year, month).stripSuffix(" ")
    val prefixes = List(calendarMonthSearchPattern format (year, month), monthlySearchPattern)
    appointmentsAndTasks(workList, calMonth, removeHeaderPrefix, prefixes: _*)
  }

  def getAppointmentsByQuarter(workList: List[String], year: Int, quarter: Int, removeHeaderPrefix: Boolean): List[ToDoList] = {
    val calQuarter = calendarQuarterSearchPattern.format(year, quarter).stripSuffix(" ")
    val prefixes = List(calendarQuarterSearchPattern format (year, quarter), quarterlySearchPattern)
    appointmentsAndTasks(workList, calQuarter, removeHeaderPrefix, prefixes: _*)
  }

  def getAppointmentsByYear(workList: List[String], year: Int, removeHeaderPrefix: Boolean): List[ToDoList] = {
    val calYear = calendarYearSearchPattern.format(year, year).stripSuffix(" ")
    val prefixes = List(calendarYearSearchPattern format (year), yearlySearchPattern)
    appointmentsAndTasks(workList, calYear, removeHeaderPrefix, prefixes: _*)
  }
}
