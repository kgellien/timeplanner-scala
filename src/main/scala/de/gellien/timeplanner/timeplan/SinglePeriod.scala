package de.gellien.timeplanner.timeplan

abstract sealed class SinglePeriod(val periodEntry: PeriodEntry, val todo: ToDoList, headerOpt: Option[String]) {
  def defaultHeader: String
  def header = headerOpt match {
    case Some(h) => h
    case None    => defaultHeader
  }
  override def toString = "%s: %s" format (periodEntry, todo)
}

// TODO: collect format strings used for toString methods at one place!

// TODO: check whether defaultHeader may become method of periodEntry - then case classes may become superfluous

case class Day(override val periodEntry: DayEntry, override val todo: ToDoList,
               headerOpt: Option[String] = None) extends SinglePeriod(periodEntry, todo, headerOpt) {
  override val defaultHeader = TimeHelper.displayDay(periodEntry.year, periodEntry.month, periodEntry.day)
}

case class Week(override val periodEntry: WeekEntry, override val todo: ToDoList,
                headerOpt: Option[String] = None) extends SinglePeriod(periodEntry, todo, headerOpt) {
  override val defaultHeader = {
    val (from, to) = TimeHelper.fromToWeek(periodEntry.year, periodEntry.week)
    "W%02d: %s -- %s" format (periodEntry.week, from, to)
  }
}

case class Month(override val periodEntry: MonthEntry, override val todo: ToDoList,
                 headerOpt: Option[String] = None) extends SinglePeriod(periodEntry, todo, headerOpt) {
  override val defaultHeader = "%s" format (TimeHelper.monthName(periodEntry.month))
}

case class Quarter(override val periodEntry: QuarterEntry, override val todo: ToDoList,
                   headerOpt: Option[String] = None) extends SinglePeriod(periodEntry, todo, headerOpt) {
  override val defaultHeader = "Q%d" format periodEntry.quarter
}

case class Year(override val periodEntry: YearEntry, override val todo: ToDoList,
                headerOpt: Option[String] = None) extends SinglePeriod(periodEntry, todo, headerOpt) {
  override val defaultHeader = "%d" format periodEntry.year
}
