package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range

class TimePlan(workList : List[String], daysPerWeekPar: Int, debug: Boolean = false) {
  implicit val daysPerWeek: Int = daysPerWeekPar
  private val periodPlanBuffer = new ListBuffer[PeriodPlan]

  def periodPlans = periodPlanBuffer.toList

  def addPeriodPlan(periodPlan: PeriodPlan) = {
    periodPlanBuffer += periodPlan
  }
  
  def addWeekPlan(year: Int, week: Int, full: Boolean = true) = {
    if (debug) println("addWeekPlan(%d-W%02d)" format (year, week))
    periodPlanBuffer += WeekPlan(year, week, workList, full)
  }

  def addWeekPlans(year: Int, weeks: Range, full: Boolean = true) = {
    for (week <- weeks)
      addWeekPlan(year, week, full)
  }

  def addMonthPlan(year: Int, month: Int, full: Boolean = true) = {
    if (debug) println("addMonthPlan(%d-%02d)" format (year, month))
    periodPlanBuffer += MonthPlan(year, month, workList, full)
  }

  def addMonthPlans(year: Int, months: Range, full: Boolean = true) = {
    for (month <- months)
      addMonthPlan(year, month, full)
  }

  def addQuarterPlan(year: Int, quarter: Int, full: Boolean = true) = {
    if (debug) println("addQuarterPlan(%d-Q%d)" format (year, quarter))
    periodPlanBuffer += QuarterPlan(year, quarter, workList, full)
  }

  def addQuarterPlans(year: Int, quarters: Range, full: Boolean = true) = {
    for (quarter <- quarters)
      addQuarterPlan(year, quarter, full)
  }

  def addYearPlan(year: Int, full: Boolean = true) = {
    if (debug) println("addYearPlan(%d)" format (year))
    periodPlanBuffer += YearPlan(year, workList, full)
  }

  def addYearPlans(years: Range, full: Boolean = true) = {
    for (year <- years)
      addYearPlan(year, full)
  }
}
