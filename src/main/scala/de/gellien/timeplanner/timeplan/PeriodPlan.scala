package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range

abstract sealed class PeriodPlan(val periodBase: PeriodEntry, val period: SinglePeriod, val periodSpecifics: List[SinglePeriod], val withOverview: Boolean) {
  val header: (String, String, String)
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
}

case class DayPlan(val periodEntry: DayEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format (TimeHelper.monthName(periodEntry.month)))
}

case class WeekPlan(val periodEntry: WeekEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = {
    val (start, end) = TimeHelper.fromToWeek(periodEntry.year, periodEntry.week)
    ("%d" format periodEntry.year, "%s -- %s" format (start, end), "W%02d" format periodEntry.week)
  }
}

case class MonthPlan(val periodEntry: MonthEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format (TimeHelper.monthName(periodEntry.month)))
}

case class QuarterPlan(val periodEntry: QuarterEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format "Q%d" format periodEntry.quarter)
}

case class YearPlan(val periodEntry: YearEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "")
}
