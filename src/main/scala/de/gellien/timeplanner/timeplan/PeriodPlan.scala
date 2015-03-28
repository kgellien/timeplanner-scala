package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range
import TimeHelper._

abstract sealed class PeriodPlan(val period: SinglePeriod, val withOverview: Boolean) {
  val header: (String, String, String)
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
  val periodSpecifics: List[SinglePeriod]
}

case class WeekPlan(year: Int, week: Int, workList: List[String], override val period: SinglePeriod, override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(period, withOverview) {
  override val header = {
    val (start, end) = TimeHelper.fromToWeek(year, week)
    ("%d" format year, "%s -- %s" format (start, end), "W%02d" format week)
  }
  override val periodSpecifics = for {
    currentDay <- daysInWeek(year, week) take daysPerWeek
    psTodos = ToDoHelper.getTodoByDay(workList, currentDay, removeHeaderPrefix = true)
  } yield Day(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth, psTodos)
}

case class MonthPlan(year: Int, month: Int, workList: List[String], override val period: SinglePeriod, override val withOverview: Boolean) extends PeriodPlan(period, withOverview) {
  override val header = ("%d" format year, "", "%s" format (TimeHelper.monthName(month)))
  override val periodSpecifics = for ((weekYear, week) <- weeksInMonth(year, month))
    yield Week(weekYear, week, ToDoHelper.getTodoByWeek(workList, weekYear, week, removeHeaderPrefix = true))
}

case class QuarterPlan(year: Int, quarter: Int, workList: List[String], override val period: SinglePeriod, override val withOverview: Boolean) extends PeriodPlan(period, withOverview) {
  override val header = ("%d" format year, "", "%s" format "Q%d" format quarter)
  override val periodSpecifics = for (month <- monthsInQuarter(year, quarter))
    yield Month(year, month, ToDoHelper.getTodoByMonth(workList, year, month, removeHeaderPrefix = true))
}

case class YearPlan(year: Int, workList: List[String], override val period: SinglePeriod, override val withOverview: Boolean) extends PeriodPlan(period, withOverview) {
  override val header = ("%d" format year, "", "")
  override val periodSpecifics = for (quarter <- (1 to 4).toList)
    yield Quarter(year, quarter, ToDoHelper.getTodoByQuarter(workList, year, quarter, removeHeaderPrefix = true))
}
