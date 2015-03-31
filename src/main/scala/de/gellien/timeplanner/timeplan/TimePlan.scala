package de.gellien.timeplanner.timeplan

abstract sealed class TimePlan() {
  def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan
}

case class WeekTimePlan(year: Int, week: Int)(implicit val daysPerWeek: Int) extends TimePlan {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByWeek(todos, year, week, removeHeaderPrefix = false)
    val period = Week(year, week, todo)
    val periodSpecifics = for {
      currentDay <- TimeHelper.daysInWeek(year, week) take daysPerWeek
      psTodos = ToDoHelper.getTodoByDay(todos, currentDay, removeHeaderPrefix = true)
    } yield Day(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth, psTodos)
    WeekPlan(year, week, period, periodSpecifics, withOverview)
  }
  override def toString = "WeekTimePlan(%d, %d)" format (year, week)
}

case class MonthTimePlan(year: Int, month: Int) extends TimePlan {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByMonth(todos, year, month, removeHeaderPrefix = false)
    val period = Month(year, month, todo)
    val periodSpecifics = for ((weekYear, week) <- TimeHelper.weeksInMonth(year, month))
      yield Week(weekYear, week, ToDoHelper.getTodoByWeek(todos, weekYear, week, removeHeaderPrefix = true))
    MonthPlan(year, month, period, periodSpecifics, withOverview)
  }
  override def toString = "MonthTimePlan(%d, %d)" format (year, month)
}

case class QuarterTimePlan(year: Int, quarter: Int) extends TimePlan {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByQuarter(todos, year, quarter, removeHeaderPrefix = false)
    val period = Quarter(year, quarter, todo)
    val periodSpecifics = for (month <- TimeHelper.monthsInQuarter(year, quarter))
      yield Month(year, month, ToDoHelper.getTodoByMonth(todos, year, month, removeHeaderPrefix = true))
    QuarterPlan(year, quarter, period, periodSpecifics, withOverview)
  }
  override def toString = "QuarterTimePlan(%d, %d)" format (year, quarter)
}

case class YearTimePlan(year: Int) extends TimePlan {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByYear(todos, year, removeHeaderPrefix = false)
    val period = Year(year, todo)
    val periodSpecifics = for (quarter <- (1 to 4).toList)
      yield Quarter(year, quarter, ToDoHelper.getTodoByQuarter(todos, year, quarter, removeHeaderPrefix = true))
    YearPlan(year, period, periodSpecifics, withOverview)
  }
  override def toString = "YearTimePlan(%d)" format (year)
}
