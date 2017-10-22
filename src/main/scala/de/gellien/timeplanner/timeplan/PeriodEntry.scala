package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

abstract sealed class PeriodBase

abstract sealed class PeriodEntry(val year: Int) extends PeriodBase {
  def withinBounds(dateBounds: List[DateBound]): Boolean = BoundChecker.withinBounds(this, dateBounds)
  val lower = TimeHelper.isoDate(TimeHelper.getFirstDayOfPeriod(this))
  val upper = TimeHelper.isoDate(TimeHelper.getLastDayOfPeriod(this))
  def header: String
}

case class DayEntry(override val year: Int, month: Int, day: Int, headerSuffix: String = "") extends PeriodEntry(year) {
  override def toString = "%d-%02d-%02d" format (year, month, day)
  override val header = TimeHelper.displayDay(year, month, day) + headerSuffix
  val isoDate = IsoDate(year, month, day)
  val localDate = new LocalDate(year, month, day)
}

case class WeekEntry(override val year: Int, week: Int) extends PeriodEntry(year) {
  override def toString = "%d-W%02d" format (year, week)
  override val header = {
    val (from, to) = TimeHelper.fromToWeek(year, week)
    "W%02d: %s - %s" format (week, from, to)
  }
}

case class MonthEntry(override val year: Int, month: Int) extends PeriodEntry(year) {
  override def toString = "%d-%02d" format (year, month)
  override val header = TimeHelper.monthName(month)
}

case class QuarterEntry(override val year: Int, quarter: Int) extends PeriodEntry(year) {
  override def toString = "%d-Q%d" format (year, quarter)
  override val header = "Q%d" format quarter
}

case class YearEntry(override val year: Int) extends PeriodEntry(year) {
  override def toString = "%d" format (year)
  override val header = year.toString
}

case class AnniversaryEntry(month: Int, day: Int) extends PeriodBase

case class WeekDayEntry(weekDay: Int) extends PeriodBase {
  override def toString = "D%d" format weekDay
}

case class DailyEntry() extends PeriodBase {
  override def toString = "D"
}
case class WeeklyEntry() extends PeriodBase {
  override def toString = "W"
}
case class MonthlyEntry() extends PeriodBase {
  override def toString = "M"
}
case class QuarterlyEntry() extends PeriodBase {
  override def toString = "Q"
}
case class YearlyEntry() extends PeriodBase {
  override def toString = "Y"
}

case class DayMonth(day: Int, month: Int)
