package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

sealed abstract class DateBound(val base: PeriodEntry) {
  // TODO find better way/names
  val lower = base.lower
  val upper = base.upper
  def valid(pe: PeriodEntry): Boolean = false
}

case class EqBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    (pe.upper <= upper) && (pe.lower >= lower)
  }
}

case class NeBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    (pe.upper < lower) || (pe.lower > upper)
  }
}

case class LtBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    val bp = pe.toString()
    val r1 = (bp < lower)
    val r2 = (!lower.startsWith(bp))
    r1 && r2
  }
}

case class GtBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    val bp = pe.toString()
    val r1 = bp > upper
    r1
  }
}

case class LeBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    val bp = pe.toString()
    val r1 = (bp <= upper)
    //    val r2 = (upper.startsWith(bp))
    //    r1 || r2
    r1
  }
}

case class GeBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    val bp = pe.toString()
    val r1 = (bp >= lower) // alone not sufficient
    val r2 = lower.startsWith(bp)
    r1 || r2
  }
}

object PeriodHelper {
  // TODO augment PeriodEntry?
  def getFirstDayOfPeriod(pe: PeriodEntry) = {
    pe match {
      case YearEntry(year)             => new LocalDate(year, 1, 1)
      case QuarterEntry(year, quarter) => new LocalDate(year, TimeHelper.monthsInQuarter(year, quarter).head, 1)
      case MonthEntry(year, month)     => new LocalDate(year, month, 1)
      case WeekEntry(year, week)       => TimeHelper.getFirstDayInWeek(year, week)
      case DayEntry(year, month, day)  => new LocalDate(year, month, day)
      case _ =>
        println("Illegal PeriodEntry: %s" format pe)
        new LocalDate(1900, 1, 1)
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
      case _ =>
        println("Illegal PeriodEntry: %s" format pe)
        new LocalDate(2100, 12, 31)
    }
  }
  def getIsoDateLowerEqBound(pe: PeriodEntry) = {
    TimeHelper.isoDate(getFirstDayOfPeriod(pe))
  }
  def getIsoDateUpperEqBound(pe: PeriodEntry) = {
    TimeHelper.isoDate(getLastDayOfPeriod(pe))
  }
}

object BoundChecker {
  def withinBounds(basePe: PeriodEntry, dateBounds: List[DateBound]) = {
    val rs = for {
      dateBound <- dateBounds
    } yield dateBound.valid(basePe)
    rs forall { _ == true }
  }
}
