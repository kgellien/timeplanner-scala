package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

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
  def withinBounds(dateBounds: List[DateBound]): Boolean
  val lower = PeriodHelper.getIsoDateLowerEqBound(this)
  val upper = PeriodHelper.getIsoDateUpperEqBound(this)
}

case class YearEntry(year: Int) extends PeriodEntry {
  override def toString = "%d" format (year)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
}

case class QuarterEntry(year: Int, quarter: Int) extends PeriodEntry {
  override def toString = "%d-Q%d" format (year, quarter)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
}

case class MonthEntry(year: Int, month: Int) extends PeriodEntry {
  override def toString = "%d-%02d" format (year, month)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
}

case class WeekEntry(year: Int, week: Int) extends PeriodEntry {
  override def toString = "%d-W%02d" format (year, week)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
}
case class DayEntry(year: Int, month: Int, day: Int) extends PeriodEntry {
  override def toString = "%d-%02d-%02d" format (year, month, day)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
}

object PeriodHelper {
  def getFirstDayOfPeriod(pe: PeriodEntry) = {
    pe match {
      case YearEntry(year)             => new LocalDate(year, 1, 1)
      case QuarterEntry(year, quarter) => new LocalDate(year, TimeHelper.monthsInQuarter(year, quarter).head, 1)
      case MonthEntry(year, month)     => new LocalDate(year, month, 1)
      case WeekEntry(year, week)       => TimeHelper.getFirstDayInWeek(year, week)
      case DayEntry(year, month, day)  => new LocalDate(year, month, day)
    }
  }
  def getLastDayOfPeriod(pe: PeriodEntry) = {
    pe match {
      case YearEntry(year) => new LocalDate(year, 12, 31)
      case QuarterEntry(year, quarter) =>
        val ld = new LocalDate(year, TimeHelper.monthsInQuarter(year, quarter).reverse.head, 1)
        ld plusMonths (1) minusDays (1)
      case MonthEntry(year, month) =>
        val ld = new LocalDate(year, month, 1)
        ld plusMonths (1) minusDays (1)
      case WeekEntry(year, week)      => TimeHelper.getFirstDayInWeek(year, week) plusDays (7)
      case DayEntry(year, month, day) => new LocalDate(year, month, day)
    }
  }
  def getIsoDateLowerEqBound(pe: PeriodEntry) = {
    TimeHelper.isoDate(getFirstDayOfPeriod(pe))
  }
  def getIsoDateUpperEqBound(pe: PeriodEntry) = {
    TimeHelper.isoDate(getLastDayOfPeriod(pe))
  }
}
