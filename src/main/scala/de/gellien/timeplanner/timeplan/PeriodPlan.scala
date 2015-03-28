package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range

abstract sealed class PeriodPlan(val period: SinglePeriod, val periodSpecifics: List[SinglePeriod], val withOverview: Boolean) {
  val header: (String, String, String)
  def periodOverview: List[SinglePeriod] = PeriodSplitter.splitPeriod(period)
}

case class WeekPlan(year: Int, week: Int, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(period, periodSpecifics, withOverview) {
  override val header = {
    val (start, end) = TimeHelper.fromToWeek(year, week)
    ("%d" format year, "%s -- %s" format (start, end), "W%02d" format week)
  }
}

case class MonthPlan(year: Int, month: Int, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(period, periodSpecifics, withOverview) {
  override val header = ("%d" format year, "", "%s" format (TimeHelper.monthName(month)))
}

case class QuarterPlan(year: Int, quarter: Int, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(period, periodSpecifics, withOverview) {
  override val header = ("%d" format year, "", "%s" format "Q%d" format quarter)
}

case class YearPlan(year: Int, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(period, periodSpecifics, withOverview) {
  override val header = ("%d" format year, "", "")
}
