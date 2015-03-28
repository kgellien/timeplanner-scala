package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range
import TimeHelper._

abstract sealed class PeriodPlan(val withOverview: Boolean) {
  val header: (String, String, String)
  val period: SinglePeriod
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
  val periodSpecifics: List[SinglePeriod]
}

case class WeekPlan(year: Int, week: Int, workList: List[String], override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(withOverview) {
  override val header = {
    val (start, end) = TimeHelper.fromToWeek(year, week)
    ("%d" format year, "%s -- %s" format (start, end), "W%02d" format week)
  }
  val todo = ToDoHelper.getTodoByWeek(workList, year, week, removeHeaderPrefix = false)
  override val period = Week(year, week, todo)
  override val periodSpecifics = for {
    currentDay <- daysInWeek(year, week) take daysPerWeek
    psTodos = ToDoHelper.getTodoByDay(workList, currentDay, removeHeaderPrefix = true)
  } yield Day(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth, psTodos)
}

case class MonthPlan(year: Int, month: Int, workList: List[String], override val withOverview: Boolean) extends PeriodPlan(withOverview) {
  override val header = ("%d" format year, "", "%s" format (TimeHelper.monthName(month)))
  val todo = ToDoHelper.getTodoByMonth(workList, year, month, removeHeaderPrefix = false)
  override val period = Month(year, month, todo)
  override val periodSpecifics = for ((weekYear, week) <- weeksInMonth(year, month))
    yield Week(weekYear, week, ToDoHelper.getTodoByWeek(workList, weekYear, week, removeHeaderPrefix = true))
}

case class QuarterPlan(year: Int, quarter: Int, workList: List[String], override val withOverview: Boolean) extends PeriodPlan(withOverview) {
  override val header = ("%d" format year, "", "%s" format "Q%d" format quarter)
  val todo = ToDoHelper.getTodoByQuarter(workList, year, quarter, removeHeaderPrefix = false)
  override val period = Quarter(year, quarter, todo)
  override val periodSpecifics = for (month <- monthsInQuarter(year, quarter))
    yield Month(year, month, ToDoHelper.getTodoByMonth(workList, year, month, removeHeaderPrefix = true))
}

case class YearPlan(year: Int, workList: List[String], override val withOverview: Boolean) extends PeriodPlan(withOverview) {
  override val header = ("%d" format year, "", "")
  val todo = ToDoHelper.getTodoByYear(workList, year, removeHeaderPrefix = false)
  override val period = Year(year, todo)
  override val periodSpecifics = for (quarter <- (1 to 4).toList)
    yield Quarter(year, quarter, ToDoHelper.getTodoByQuarter(workList, year, quarter, removeHeaderPrefix = true))
}
