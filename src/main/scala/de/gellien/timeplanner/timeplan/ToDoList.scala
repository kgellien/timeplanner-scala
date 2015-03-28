package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate
import TimeHelper._

case class ToDoList(val anniversaries: List[String], val appointments: List[String], val tasks: List[String])

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

  def getTodoByDay(workList: List[String], currentDay: LocalDate, removeHeaderPrefix: Boolean): ToDoList = {
    val anniversaries = getEntriesWithPrefix(workList, isoDate(currentDay).substring(5) + " ")
    val prefixes = List(isoDate(currentDay) + " ", dailySearchPattern, currentDay.getDayOfWeek().toString + " ")
    val (appointments, tasks) = appointmentsAndTasks(workList, isoDate(currentDay), removeHeaderPrefix, prefixes: _*)
    ToDoList(anniversaries, appointments, tasks)
  }

  def getTodoByWeek(workList: List[String], year: Int, week: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calWeek = calendarWeekSearchPattern.format(year, week).stripSuffix(" ")
    val prefixes = List(calendarWeekSearchPattern format (year, week), weeklySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calWeek, removeHeaderPrefix, prefixes: _*)
    val anniversaries = List()
    ToDoList(anniversaries, appointments, tasks)
  }

  def getTodoByMonth(workList: List[String], year: Int, month: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calMonth = calendarMonthSearchPattern.format(year, month).stripSuffix(" ")
    val prefixes = List(calendarMonthSearchPattern format (year, month), monthlySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calMonth, removeHeaderPrefix, prefixes: _*)
    ToDoList(List(), appointments, tasks)
  }

  def getTodoByQuarter(workList: List[String], year: Int, quarter: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calQuarter = calendarQuarterSearchPattern.format(year, quarter).stripSuffix(" ")
    val prefixes = List(calendarQuarterSearchPattern format (year, quarter), quarterlySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calQuarter, removeHeaderPrefix, prefixes: _*)
    ToDoList(List(), appointments, tasks)
  }

  def getTodoByYear(workList: List[String], year: Int, removeHeaderPrefix: Boolean): ToDoList = {
    val calYear = calendarYearSearchPattern.format(year, year).stripSuffix(" ")
    val prefixes = List(calendarYearSearchPattern format (year), yearlySearchPattern)
    val (appointments, tasks) = appointmentsAndTasks(workList, calYear, removeHeaderPrefix, prefixes: _*)
    ToDoList(List(), appointments, tasks)
  }
}
