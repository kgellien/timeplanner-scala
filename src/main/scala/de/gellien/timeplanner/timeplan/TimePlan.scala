package de.gellien.timeplanner.timeplan

abstract sealed class TimePlan(val periodEntry: PeriodEntry) {
  def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan
}

case class DayTimePlan(override val periodEntry: DayEntry)(implicit val daysPerWeek: Int) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByDay(todos, periodEntry, removeHeaderPrefix = false)
    val period = Day(periodEntry, todo)
    val periodSpecifics = List()
    DayPlan(periodEntry, period, periodSpecifics, withOverview)
  }
  override def toString = "DayTimePlan(%s)" format (periodEntry)
}

case class WeekTimePlan(override val periodEntry: WeekEntry)(implicit val daysPerWeek: Int) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByWeek(todos, periodEntry, removeHeaderPrefix = false)
    val period = Week(periodEntry, todo)
    val periodSpecifics = for {
      currentDay <- TimeHelper.daysInWeek(periodEntry.year, periodEntry.week) take daysPerWeek
      pe = DayEntry(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth)
      psTodos = ToDoHelper.getTodoByDay(todos, pe, removeHeaderPrefix = true)
    } yield Day(pe, psTodos)
    WeekPlan(periodEntry, period, periodSpecifics, withOverview)
  }
  override def toString = "WeekTimePlan(%s)" format (periodEntry)
}

case class MonthTimePlan(override val periodEntry: MonthEntry) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByMonth(todos, periodEntry, removeHeaderPrefix = false)
    val period = Month(periodEntry, todo)
    val periodSpecifics = for {
      (weekYear, week) <- TimeHelper.weeksInMonth(periodEntry.year, periodEntry.month)
      pe = WeekEntry(weekYear, week)
    } yield Week(pe, ToDoHelper.getTodoByWeek(todos, pe, removeHeaderPrefix = true))
    MonthPlan(periodEntry, period, periodSpecifics, withOverview)
  }
  override def toString = "MonthTimePlan(%s)" format (periodEntry)
}

case class QuarterTimePlan(override val periodEntry: QuarterEntry) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByQuarter(todos, periodEntry, removeHeaderPrefix = false)
    val period = Quarter(periodEntry, todo)
    val periodSpecifics = for (month <- TimeHelper.monthsInQuarter(periodEntry.year, periodEntry.quarter); pe = MonthEntry(periodEntry.year, month))
      yield Month(pe, ToDoHelper.getTodoByMonth(todos, pe, removeHeaderPrefix = true))
    QuarterPlan(periodEntry, period, periodSpecifics, withOverview)
  }
  override def toString = "QuarterTimePlan(%s)" format (periodEntry)
}

case class YearTimePlan(override val periodEntry: YearEntry) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDoEntry], withOverview: Boolean): PeriodPlan = {
    val todo = ToDoHelper.getTodoByYear(todos, periodEntry, removeHeaderPrefix = false)
    val period = Year(periodEntry, todo)
    val periodSpecifics = for (quarter <- (1 to 4).toList; pe = QuarterEntry(periodEntry.year, quarter))
      yield Quarter(pe, ToDoHelper.getTodoByQuarter(todos, pe, removeHeaderPrefix = true))
    YearPlan(periodEntry, period, periodSpecifics, withOverview)
  }
  override def toString = "YearTimePlan(%s)" format (periodEntry)
}
