package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range

class TimePlan() {
  private val periodPlanBuffer = new ListBuffer[PeriodPlan]

  def periodPlans = periodPlanBuffer.toList

  def addPeriodPlan(periodPlan: PeriodPlan) = {
    periodPlanBuffer += periodPlan
  }
}
