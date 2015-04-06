package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

sealed abstract class DateBound(val pe: PeriodEntry) {
  // TODO find better way/names
  def lower: String
  def upper: String
}

case class EqBound(override val pe: PeriodEntry) extends DateBound(pe) {
  val lower = PeriodHelper.getIsoDateLowerEqBound(pe)
  val upper = PeriodHelper.getIsoDateUpperEqBound(pe)
}

case class NeBound(override val pe: PeriodEntry) extends DateBound(pe) {
  val lower = PeriodHelper.getIsoDateLowerEqBound(pe)
  val upper = PeriodHelper.getIsoDateUpperEqBound(pe)
}

case class LtBound(override val pe: PeriodEntry) extends DateBound(pe) {
  val lower = PeriodHelper.getIsoDateLowerEqBound(pe)
  val upper = PeriodHelper.getIsoDateUpperEqBound(pe)
}

case class GtBound(override val pe: PeriodEntry) extends DateBound(pe) {
  val lower = PeriodHelper.getIsoDateLowerEqBound(pe)
  val upper = PeriodHelper.getIsoDateUpperEqBound(pe)
}

case class LeBound(override val pe: PeriodEntry) extends DateBound(pe) {
  val lower = PeriodHelper.getIsoDateLowerEqBound(pe)
  val upper = PeriodHelper.getIsoDateUpperEqBound(pe)
}

case class GeBound(override val pe: PeriodEntry) extends DateBound(pe) {
  val lower = PeriodHelper.getIsoDateLowerEqBound(pe)
  val upper = PeriodHelper.getIsoDateUpperEqBound(pe)
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
  // TODO: as method of DateBound
  def withinBounds(basePe: PeriodEntry, dateBounds: List[DateBound]) = {
    val rs = for {
      dateBound <- dateBounds
      res = dateBound match {
        case EqBound(pe) => (basePe.toString() <= dateBound.upper) && (basePe.toString() >= dateBound.lower)
        case NeBound(pe) => (basePe.toString() <= dateBound.lower) && (basePe.toString() >= dateBound.upper)
        case LtBound(pe) => (basePe.toString() < dateBound.lower) && (!dateBound.lower.startsWith(basePe.toString()))
        case LeBound(pe) => basePe.toString() <= dateBound.lower
        case GtBound(pe) => basePe.toString() > dateBound.upper
        case GeBound(pe) =>
          val r = (basePe.toString() >= dateBound.lower) || (dateBound.lower.startsWith(basePe.toString()))
          //          println("      %s >= %s = %s" format (basePe.toString(), dateBound.lower, r))
          r
      }
    } yield {
      print("    (%s,  %s) " format (dateBound.lower, dateBound.upper))
      res
    }
    val result = rs forall { _ == true }
    print("%s withinBound %s => " format (basePe, dateBounds))
    println(result)
    result
  }
}
