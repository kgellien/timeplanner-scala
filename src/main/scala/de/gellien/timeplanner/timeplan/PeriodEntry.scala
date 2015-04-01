package de.gellien.timeplanner.timeplan

abstract sealed class PeriodEntry

case class AnniversaryEntry(month: Int, day: Int) extends PeriodEntry

abstract sealed class YearBase extends PeriodEntry
case class YearlyEntry() extends YearBase {
  override def toString = "Y"
}
case class YearEntry(year: Int) extends YearBase {
  override def toString = "%d" format (year)
}

abstract sealed class QuarterBase extends PeriodEntry
case class QuarterlyEntry() extends QuarterBase {
  override def toString = "Q"
}
case class QuarterEntry(year: Int, quarter: Int) extends QuarterBase {
  override def toString = "%d-Q%d" format (year, quarter)
}

abstract sealed class MonthBase extends PeriodEntry
case class MonthlyEntry() extends MonthBase {
  override def toString = "M"
}
case class MonthEntry(year: Int, month: Int) extends MonthBase {
  override def toString = "%d-%02d" format (year, month)
}

abstract sealed class WeekBase extends PeriodEntry
case class WeeklyEntry() extends WeekBase {
  override def toString = "W"
}
case class WeekEntry(year: Int, week: Int) extends WeekBase {
  override def toString = "%d-W%02d" format (year, week)
}

abstract sealed class DayBase extends PeriodEntry
case class DailyEntry() extends DayBase {
  override def toString = "D"
}
case class DayEntry(year: Int, month: Int, day: Int) extends DayBase {
  override def toString = "%d-%02d-%02d" format (year, month, day)
}
case class WeekDayEntry(weekDay: Int) extends DayBase {
  override def toString = "D%d" format weekDay
}
