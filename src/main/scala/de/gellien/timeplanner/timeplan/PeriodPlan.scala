package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate
import TimeHelper._

//sealed abstract case class ToDos()

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range

class PeriodPlans() {
  private val periodPlanBuffer = new ListBuffer[PeriodPlan]

  def periodPlans = periodPlanBuffer.toList

  def addPeriodPlan(periodPlan: PeriodPlan) = {
    periodPlanBuffer += periodPlan
  }
}

abstract class PeriodPlan(val withOverview: Boolean) {
  val period: SinglePeriod
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
  val periodSpecifics: List[SinglePeriod]
}

case class WeekPlan(year: Int, week: Int, workList: List[String], override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(withOverview) {
  val todo = PeriodPlan.getTodoByWeek(workList, year, week, removeHeaderPrefix = false)
  override val period = Week(year, week, todo)
  override val periodSpecifics = for {
    currentDay <- daysInWeek(year, week) take daysPerWeek
    psTodos = PeriodPlan.getTodoByDay(workList, currentDay, removeHeaderPrefix = true)
  } yield Day(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth, psTodos)
}

case class MonthPlan(year: Int, month: Int, workList: List[String], override val withOverview: Boolean) extends PeriodPlan(withOverview) {
  val todo = PeriodPlan.getTodoByMonth(workList, year, month, removeHeaderPrefix = false)
  override val period = Month(year, month, todo)
  override val periodSpecifics = for ((weekYear, week) <- weeksInMonth(year, month))
    yield Week(weekYear, week, PeriodPlan.getTodoByWeek(workList, weekYear, week, removeHeaderPrefix = true))
}

case class QuarterPlan(year: Int, quarter: Int, workList: List[String], override val withOverview: Boolean) extends PeriodPlan(withOverview) {
  val todo = PeriodPlan.getTodoByQuarter(workList, year, quarter, removeHeaderPrefix = false)
  override val period = Quarter(year, quarter, todo)
  override val periodSpecifics = for (month <- monthsInQuarter(year, quarter))
    yield Month(year, month, PeriodPlan.getTodoByMonth(workList, year, month, removeHeaderPrefix = true))
}

case class YearPlan(year: Int, workList: List[String], override val withOverview: Boolean) extends PeriodPlan(withOverview) {
  val todo = PeriodPlan.getTodoByYear(workList, year, removeHeaderPrefix = false)
  override val period = Year(year, todo)
  override val periodSpecifics = for (quarter <- (1 to 4).toList)
    yield Quarter(year, quarter, PeriodPlan.getTodoByQuarter(workList, year, quarter, removeHeaderPrefix = true))
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
