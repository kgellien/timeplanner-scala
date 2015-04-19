package de.gellien.timeplanner.timeplan

abstract sealed class PeriodBase

abstract sealed class PeriodEntry extends PeriodBase {
  def withinBounds(dateBounds: List[DateBound]): Boolean = BoundChecker.withinBounds(this, dateBounds)
  val lower = TimeHelper.isoDate(TimeHelper.getFirstDayOfPeriod(this))
  val upper = TimeHelper.isoDate(TimeHelper.getLastDayOfPeriod(this))
  def header: String
}

case class DayEntry(year: Int, month: Int, day: Int) extends PeriodEntry {
  override def toString = "%d-%02d-%02d" format (year, month, day)
  override val header = TimeHelper.displayDay(year, month, day)
}

case class WeekEntry(year: Int, week: Int) extends PeriodEntry {
  override def toString = "%d-W%02d" format (year, week)
  override val header = {
    val (from, to) = TimeHelper.fromToWeek(year, week)
    "W%02d: %s -- %s" format (week, from, to)
  }
}

case class MonthEntry(year: Int, month: Int) extends PeriodEntry {
  override def toString = "%d-%02d" format (year, month)
  override val header = "%s" format (TimeHelper.monthName(month))
}

case class QuarterEntry(year: Int, quarter: Int) extends PeriodEntry {
  override def toString = "%d-Q%d" format (year, quarter)
  override val header = "Q%d" format quarter
}

case class YearEntry(year: Int) extends PeriodEntry {
  override def toString = "%d" format (year)
  override val header = "%d" format year
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
