package de.gellien.timeplanner.timeplan

abstract sealed class TimePlan() {
  def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan
}

case class WeekTimePlan(year: Int, week: Int)(implicit val daysPerWeek: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByWeek(workList, year, week, removeHeaderPrefix = false)
    val period = Week(year, week, todo)
    WeekPlan(year, week, workList, period, withOverview)
  }
  override def toString = "WeekTimePlan(%d, %d)" format (year, week)
}

case class MonthTimePlan(year: Int, month: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByMonth(workList, year, month, removeHeaderPrefix = false)
    val period = Month(year, month, todo)
    MonthPlan(year, month, workList, period, withOverview)
  }
  override def toString = "MonthTimePlan(%d, %d)" format (year, month)
}

case class QuarterTimePlan(year: Int, quarter: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
  val todo = ToDoHelper.getTodoByQuarter(workList, year, quarter, removeHeaderPrefix = false)
   val period = Quarter(year, quarter, todo)
    QuarterPlan(year, quarter, workList, period, withOverview)
  }
  override def toString = "QuarterTimePlan(%d, %d)" format (year, quarter)
}

case class YearTimePlan(year: Int) extends TimePlan {
  override def createPeriodPlan(workList: List[String], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByYear(workList, year, removeHeaderPrefix = false)
    val period = Year(year, todo)
    YearPlan(year, workList, period, withOverview)
  }
  override def toString = "YearTimePlan(%d)" format (year)
}
