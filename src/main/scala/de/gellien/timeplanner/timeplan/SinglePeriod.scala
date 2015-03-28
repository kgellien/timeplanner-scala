package de.gellien.timeplanner.timeplan

abstract sealed class SinglePeriod(val todo: ToDoList, val hheader: Option[String]) {
  def defaultHeader: String
  def header = hheader match {
    case Some(h) => h
    case None    => defaultHeader
  }
}

// TODO: collect format strings used for toString methods at one place!

case class Day(year: Int, month: Int, day: Int, override val todo: ToDoList,
               override val hheader: Option[String] = None) extends SinglePeriod(todo, hheader) {
  override val defaultHeader = TimeHelper.displayDay(year, month, day)
  override def toString = "%4d-%02d-%02d: %s" format (year, month, day, todo)
}

case class Week(year: Int, week: Int, override val todo: ToDoList,
                override val hheader: Option[String] = None) extends SinglePeriod(todo, hheader) {
  override val defaultHeader = {
    val (from, to) = TimeHelper.fromToWeek(year, week)
    val fromTo = "%s -- %s" format (from, to)
    "W%02d: %s" format (week, fromTo)
  }
  override def toString = "%4d-W%02d: %s" format (year, week, todo)
}

case class Month(year: Int, month: Int, override val todo: ToDoList,
                 override val hheader: Option[String] = None) extends SinglePeriod(todo, hheader) {
  override val defaultHeader = "%s" format (TimeHelper.monthName(month))
  override def toString = "%4d-%02d: %s" format (year, month, todo)
}

case class Quarter(year: Int, quarter: Int, override val todo: ToDoList,
                   override val hheader: Option[String] = None) extends SinglePeriod(todo, hheader) {
  override val defaultHeader = "Q%d" format quarter
  override def toString = "%4d-Q%d: %s" format (year, quarter, todo)
}

case class Year(year: Int, override val todo: ToDoList,
                override val hheader: Option[String] = None) extends SinglePeriod(todo, hheader) {
  override val defaultHeader = "%d" format year
  override def toString = "%4d: %s" format (year, todo)
}
