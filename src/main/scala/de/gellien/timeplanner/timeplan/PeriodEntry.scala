package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

object BoundChecker {
  def withinBounds(basePe: PeriodEntry, dateBounds: List[DateBound]) = {
    val rs = for {
      dateBound <- dateBounds
      res = dateBound match {
        case EqBound(pe) => basePe within pe
        case NeBound(pe) => !(basePe within pe)
        case LtBound(pe) => basePe.toString() < pe.toString()
        case LeBound(pe) => basePe.toString() <= pe.toString()
        case GtBound(pe) => basePe.toString() > pe.toString()
        case GeBound(pe) => basePe.toString() >= pe.toString()
      }
    } yield res
    rs forall { _ == true }
  }
}

abstract sealed class PeriodEntry {
  def withinBounds(dateBounds: List[DateBound]) = false
  def within(pe: PeriodEntry) = false
}

case class AnniversaryEntry(month: Int, day: Int) extends PeriodEntry

abstract sealed class YearBase extends PeriodEntry
case class YearlyEntry() extends YearBase {
  override def toString = "Y"
}
case class YearEntry(year: Int) extends YearBase {
  override def toString = "%d" format (year)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
  override def within(pe: PeriodEntry) = {
    val result = pe match {
      case YearEntry(y) => year == y
      case _            => false
    }
    //println("%s <-> %s %s" format (this, pe, result))
    result
  }
}

abstract sealed class QuarterBase extends PeriodEntry
case class QuarterlyEntry() extends QuarterBase {
  override def toString = "Q"
}
case class QuarterEntry(year: Int, quarter: Int) extends QuarterBase {
  override def toString = "%d-Q%d" format (year, quarter)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
  override def within(pe: PeriodEntry) = {
    val result = pe match {
      case YearEntry(y)       => year == y
      case QuarterEntry(y, q) => (year == y) && (quarter == q)
      case _                  => false
    }
    //println("%s <-> %s %s" format (this, pe, result))
    result
  }
}

abstract sealed class MonthBase extends PeriodEntry
case class MonthlyEntry() extends MonthBase {
  override def toString = "M"
}
case class MonthEntry(year: Int, month: Int) extends MonthBase {
  override def toString = "%d-%02d" format (year, month)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
  override def within(pe: PeriodEntry) = {
    val result = pe match {
      case YearEntry(y)       => year == y
      case QuarterEntry(y, q) => (year == y) && (TimeHelper.monthsInQuarter(y, q) contains (year, month))
      case MonthEntry(y, m)   => (year == y) && (month == m)
      case _                  => false
    }
    //println("%s <-> %s %s" format (this, pe, result))
    result
  }
}

abstract sealed class WeekBase extends PeriodEntry
case class WeeklyEntry() extends WeekBase {
  override def toString = "W"
}
case class WeekEntry(year: Int, week: Int) extends WeekBase {
  override def toString = "%d-W%02d" format (year, week)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
  override def within(pe: PeriodEntry) = {
    val result = pe match {
      case YearEntry(y)       => year == y
      case QuarterEntry(y, q) => (year == y) && (TimeHelper.weeksInQuarter(y, q) contains (year, week))
      case MonthEntry(y, m)   => TimeHelper.weeksInMonth(y, m) contains (year, week)
      case WeekEntry(y, w)    => (year == y) && (week == w)
      case _                  => false
    }
    //println("%s <-> %s %s" format (this, pe, result))
    result
  }
}

abstract sealed class DayBase extends PeriodEntry
case class DailyEntry() extends DayBase {
  override def toString = "D"
}
case class DayEntry(year: Int, month: Int, day: Int) extends DayBase {
  override def toString = "%d-%02d-%02d" format (year, month, day)
  override def withinBounds(dateBounds: List[DateBound]) = BoundChecker.withinBounds(this, dateBounds)
  override def within(pe: PeriodEntry) = {
    val result = pe match {
      case YearEntry(y)       => year == y
      case QuarterEntry(y, q) => (year == y) && (TimeHelper.monthsInQuarter(y, q) contains month)
      case MonthEntry(y, m)   => (year == y) && (month == m)
      case WeekEntry(y, w) => {
        val wim = TimeHelper.daysInWeek(y, w) map { _.toString }
        val dayLd = new LocalDate(year, month, day).toString
        wim contains dayLd
      }
      case DayEntry(y, m, d) => (year == y) && (month == m) && (day == d)
      case _                 => false
    }
    //println("%s <-> %s %s" format (this, pe, result))
    result
  }
}
case class WeekDayEntry(weekDay: Int) extends DayBase {
  override def toString = "D%d" format weekDay
}
