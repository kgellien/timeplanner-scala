package de.gellien.timeplanner.timeplan

case class SinglePeriod(val periodEntry: PeriodEntry, val todo: ToDoList, header: String) {
  override def toString = "%s: %s" format (periodEntry, todo)
}

abstract sealed class PeriodPlan(val periodEntry: PeriodEntry, val period: SinglePeriod, val periodSpecifics: List[SinglePeriod], val withOverview: Boolean) {
  val header: (String, String, String)
  def periodOverview: List[SinglePeriod] = PeriodSplitter(period)
}

case class DayPlan(override val periodEntry: DayEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format (TimeHelper.monthName(periodEntry.month)))
}

case class WeekPlan(override val periodEntry: WeekEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean)(implicit val daysPerWeek: Int) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = {
    val (start, end) = TimeHelper.fromToWeek(periodEntry.year, periodEntry.week)
    ("%d" format periodEntry.year, "%s -- %s" format (start, end), "W%02d" format periodEntry.week)
  }
}

case class MonthPlan(override val periodEntry: MonthEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format (TimeHelper.monthName(periodEntry.month)))
}

case class QuarterPlan(override val periodEntry: QuarterEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "%s" format "Q%d" format periodEntry.quarter)
}

case class YearPlan(override val periodEntry: YearEntry, override val period: SinglePeriod, override val periodSpecifics: List[SinglePeriod], override val withOverview: Boolean) extends PeriodPlan(periodEntry, period, periodSpecifics, withOverview) {
  override val header = ("%d" format periodEntry.year, "", "")
}

object PeriodPlan {
  def apply(pe: PeriodEntry, todos: List[ToDo], withOverview: Boolean)(implicit daysPerWeek: Int, withAdditionalTasks: Boolean): PeriodPlan = {
    pe match {
      case periodEntry @ DayEntry(y, m, d) => {
        val pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(periodEntry)), AnniversaryEntry(periodEntry.month, periodEntry.day))
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, pes: _*)
        val period = SinglePeriod(periodEntry, todoList, periodEntry.header)
        val periodSpecifics = List()
        DayPlan(periodEntry, period, periodSpecifics, withOverview)
      }
      case periodEntry @ WeekEntry(y, w) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, WeeklyEntry())
        val period = SinglePeriod(periodEntry, todoList, periodEntry.header)
        val additionalTasks = if (withAdditionalTasks) for {
          appointment <- todoList.appointments
          additionalTask <- appointment.extractSubTasks()
        } yield additionalTask
        else Nil
        val augmentedTodos = todos ++ additionalTasks
        val periodSpecifics = for {
          currentDay <- TimeHelper.daysInWeek(periodEntry.year, periodEntry.week) take daysPerWeek
          pe = DayEntry(currentDay.getYear, currentDay.getMonthOfYear, currentDay.getDayOfMonth)
          pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(pe)), AnniversaryEntry(pe.month, pe.day))
          psTodos = ToDoHelper.extractTodosForPeriod(pe, augmentedTodos, pes: _*)
        } yield SinglePeriod(pe, psTodos, pe.header)
        WeekPlan(periodEntry, period, periodSpecifics, withOverview)
      }
      case periodEntry @ MonthEntry(y, m) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, MonthlyEntry())
        val period = SinglePeriod(periodEntry, todoList, periodEntry.header)
        val periodSpecifics = for {
          (weekYear, week) <- TimeHelper.weeksInMonth(periodEntry.year, periodEntry.month)
          pe = WeekEntry(weekYear, week)
        } yield SinglePeriod(pe, ToDoHelper.extractTodosForPeriod(pe, todos, WeeklyEntry()), pe.header)
        MonthPlan(periodEntry, period, periodSpecifics, withOverview)
      }
      case periodEntry @ QuarterEntry(y, q) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, QuarterlyEntry())
        val period = SinglePeriod(periodEntry, todoList, periodEntry.header)
        val periodSpecifics = for {
          month <- TimeHelper.monthsInQuarter(periodEntry.year, periodEntry.quarter)
          pe = MonthEntry(periodEntry.year, month)
        } yield SinglePeriod(pe, ToDoHelper.extractTodosForPeriod(pe, todos, MonthlyEntry()), pe.header)
        QuarterPlan(periodEntry, period, periodSpecifics, withOverview)
      }
      case periodEntry @ YearEntry(y) => {
        val todoList = ToDoHelper.extractTodosForPeriod(periodEntry, todos, YearlyEntry())
        val period = SinglePeriod(periodEntry, todoList, periodEntry.header)
        val periodSpecifics = for {
          quarter <- (1 to 4).toList
          pe = QuarterEntry(periodEntry.year, quarter)
        } yield SinglePeriod(pe, ToDoHelper.extractTodosForPeriod(pe, todos, QuarterlyEntry()), pe.header)
        YearPlan(periodEntry, period, periodSpecifics, withOverview)
      }
    }
  }
}
