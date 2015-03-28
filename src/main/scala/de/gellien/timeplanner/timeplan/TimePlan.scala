package de.gellien.timeplanner.timeplan

abstract sealed class TimePlan() {
  def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan
}

case class WeekTimePlan(year: Int, week: Int)(implicit val daysPerWeek: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    WeekPlan(year, week, workList, withOverview)
  }
  override def toString = "WeekTimePlan(%d, %d)" format (year, week)
}

case class MonthTimePlan(year: Int, month: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    MonthPlan(year, month, workList, withOverview)
  }
  override def toString = "MonthTimePlan(%d, %d)" format (year, month)
}

case class QuarterTimePlan(year: Int, quarter: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    QuarterPlan(year, quarter, workList, withOverview)
  }
  override def toString = "QuarterTimePlan(%d, %d)" format (year, quarter)
}

case class YearTimePlan(year: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    YearPlan(year, workList, withOverview)
  }
  override def toString = "YearTimePlan(%d)" format (year)
}
