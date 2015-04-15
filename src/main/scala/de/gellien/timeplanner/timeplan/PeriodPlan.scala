package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range

abstract sealed class PeriodPlan(val periodEntry: PeriodEntry, val period: SinglePeriod, val periodSpecifics: List[SinglePeriod], val withOverview: Boolean) {
  val header: (String, String, String)
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
}

case class DayPlan(override val periodEntry: DayEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format (TimeHelper.monthName(periodEntry.month)))
}

case class WeekPlan(override val periodEntry: WeekEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = {
    val (start, end) = TimeHelper.fromToWeek(periodEntry.year, periodEntry.week)
    ("%d" format periodEntry.year, "%s -- %s" format (start, end), "W%02d" format periodEntry.week)
  }
}

case class MonthPlan(override val periodEntry: MonthEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format (TimeHelper.monthName(periodEntry.month)))
}

case class QuarterPlan(override val periodEntry: QuarterEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format "Q%d" format periodEntry.quarter)
}

case class YearPlan(override val periodEntry: YearEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "")
}

object PeriodPlan {
  def apply(pe: PeriodEntry, todos: List[ToDo], withOverview: Boolean)(implicit daysPerWeek: Int): PeriodPlan = {
    pe match {
      case periodEntry @ DayEntry(y, m, d) => {
        val pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(periodEntry)), AnniversaryEntry(periodEntry.month, periodEntry.day))
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, pes: _*)
        val periodSpecifics = List()
        DayPlan(periodEntry, Day(periodEntry, todoList), periodSpecifics, withOverview)
      }
      case periodEntry @ WeekEntry(y, w) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, WeeklyEntry())
        val periodSpecifics = for {
          currentDay <- TimeHelper.daysInWeek(periodEntry.year, periodEntry.week) take daysPerWeek
          pe = DayEntry(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth)
          pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(pe)), AnniversaryEntry(pe.month, pe.day))
          psTodos = ToDoHelper.extractTodosForPeriod(pe, todos, pes: _*)
        } yield Day(pe, psTodos)
        WeekPlan(periodEntry, Week(periodEntry, todoList), periodSpecifics, withOverview)
      }
      case periodEntry @ MonthEntry(y, m) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, MonthlyEntry())
        val periodSpecifics = for {
          (weekYear, week) <- TimeHelper.weeksInMonth(periodEntry.year, periodEntry.month)
          pe = WeekEntry(weekYear, week)
        } yield Week(pe, ToDoHelper.extractTodosForPeriod(pe, todos, WeeklyEntry()))
        MonthPlan(periodEntry, Month(periodEntry, todoList), periodSpecifics, withOverview)
      }
      case periodEntry @ QuarterEntry(y, q) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, QuarterlyEntry())
        val periodSpecifics = for {
          month <- TimeHelper.monthsInQuarter(periodEntry.year, periodEntry.quarter)
          pe = MonthEntry(periodEntry.year, month)
        } yield Month(pe, ToDoHelper.extractTodosForPeriod(pe, todos, MonthlyEntry()))
        QuarterPlan(periodEntry, Quarter(periodEntry, todoList), periodSpecifics, withOverview)
      }
      case periodEntry @ YearEntry(y) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, YearlyEntry())
        val periodSpecifics = for {
          quarter <- (1 to 4).toList
          pe = QuarterEntry(periodEntry.year, quarter)
        } yield Quarter(pe, ToDoHelper.extractTodosForPeriod(pe, todos, QuarterlyEntry()))
        YearPlan(periodEntry, Year(periodEntry, todoList), periodSpecifics, withOverview)
      }
    }
  }
}
