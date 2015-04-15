package de.gellien.timeplanner.timeplan

abstract sealed class PeriodBase

case class AnniversaryEntry(month: Int, day: Int) extends PeriodBase

case class YearlyEntry() extends PeriodBase {
  override def toString = "Y"
}
case class QuarterlyEntry() extends PeriodBase {
  override def toString = "Q"
}
case class MonthlyEntry() extends PeriodBase {
  override def toString = "M"
}
case class WeeklyEntry() extends PeriodBase {
  override def toString = "W"
}
case class DailyEntry() extends PeriodBase {
  override def toString = "D"
}
case class WeekDayEntry(weekDay: Int) extends PeriodBase {
  override def toString = "D%d" format weekDay
}

abstract sealed class PeriodEntry extends PeriodBase {
  def withinBounds(dateBounds: List[DateBound]): Boolean = BoundChecker.withinBounds(this, dateBounds)
  val lower = TimeHelper.isoDate(TimeHelper.getFirstDayOfPeriod(this))
  val upper = TimeHelper.isoDate(TimeHelper.getLastDayOfPeriod(this))
}

case class YearEntry(year: Int) extends PeriodEntry {
  override def toString = "%d" format (year)
}

case class QuarterEntry(year: Int, quarter: Int) extends PeriodEntry {
  override def toString = "%d-Q%d" format (year, quarter)
}

case class MonthEntry(year: Int, month: Int) extends PeriodEntry {
  override def toString = "%d-%02d" format (year, month)
}

case class WeekEntry(year: Int, week: Int) extends PeriodEntry {
  override def toString = "%d-W%02d" format (year, week)
}
case class DayEntry(year: Int, month: Int, day: Int) extends PeriodEntry {
  override def toString = "%d-%02d-%02d" format (year, month, day)
}
