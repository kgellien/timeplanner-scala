package de.gellien.timeplanner.timeplan

abstract sealed class TimePlan(val periodEntry: PeriodEntry) {
  def createPeriodPlan(todos: List[ToDo], withOverview: Boolean): PeriodPlan
}

case class DayTimePlan(override val periodEntry: DayEntry)(implicit val daysPerWeek: Int) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDo], withOverview: Boolean): PeriodPlan = {
    val pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(periodEntry)), AnniversaryEntry(periodEntry.month, periodEntry.day))
    val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, pes:_*)
    val periodSpecifics = List()
    DayPlan(periodEntry, Day(periodEntry, todoList), periodSpecifics, withOverview)
  }
  override def toString = "DayTimePlan(%s)" format (periodEntry)
}

case class WeekTimePlan(override val periodEntry: WeekEntry)(implicit val daysPerWeek: Int) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDo], withOverview: Boolean): PeriodPlan = {
    val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, WeeklyEntry())
    val periodSpecifics = for {
      currentDay <- TimeHelper.daysInWeek(periodEntry.year, periodEntry.week) take daysPerWeek
      pe = DayEntry(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth)
      pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(pe)), AnniversaryEntry(pe.month, pe.day))
      psTodos = ToDoHelper.extractTodosForPeriod(pe, todos, pes:_*)
    } yield Day(pe, psTodos)
    WeekPlan(periodEntry, Week(periodEntry, todoList), periodSpecifics, withOverview)
  }
  override def toString = "WeekTimePlan(%s)" format (periodEntry)
}

case class MonthTimePlan(override val periodEntry: MonthEntry) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDo], withOverview: Boolean): PeriodPlan = {
    val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, MonthlyEntry())
    val periodSpecifics = for {
      (weekYear, week) <- TimeHelper.weeksInMonth(periodEntry.year, periodEntry.month)
      pe = WeekEntry(weekYear, week)
    } yield Week(pe, ToDoHelper.extractTodosForPeriod(pe, todos, WeeklyEntry()))
    MonthPlan(periodEntry, Month(periodEntry, todoList), periodSpecifics, withOverview)
  }
  override def toString = "MonthTimePlan(%s)" format (periodEntry)
}

case class QuarterTimePlan(override val periodEntry: QuarterEntry) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDo], withOverview: Boolean): PeriodPlan = {
    val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, QuarterlyEntry())
    val periodSpecifics = for {
      month <- TimeHelper.monthsInQuarter(periodEntry.year, periodEntry.quarter)
      pe = MonthEntry(periodEntry.year, month)
    } yield Month(pe, ToDoHelper.extractTodosForPeriod(pe, todos, MonthlyEntry()))
    QuarterPlan(periodEntry, Quarter(periodEntry, todoList), periodSpecifics, withOverview)
  }
  override def toString = "QuarterTimePlan(%s)" format (periodEntry)
}

case class YearTimePlan(override val periodEntry: YearEntry) extends TimePlan(periodEntry) {
  override def createPeriodPlan(todos: List[ToDo], withOverview: Boolean): PeriodPlan = {
    val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, YearlyEntry())
    val periodSpecifics = for {
      quarter <- (1 to 4).toList
      pe = QuarterEntry(periodEntry.year, quarter)
    } yield Quarter(pe, ToDoHelper.extractTodosForPeriod(pe, todos, QuarterlyEntry()))
    YearPlan(periodEntry, Year(periodEntry, todoList), periodSpecifics, withOverview)
  }
  override def toString = "YearTimePlan(%s)" format (periodEntry)
}
