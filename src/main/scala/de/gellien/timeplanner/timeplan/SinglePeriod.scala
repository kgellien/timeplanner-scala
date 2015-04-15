package de.gellien.timeplanner.timeplan

case class SinglePeriod(val periodEntry: PeriodEntry, val todo: ToDoList, headerOpt: Option[String] = None) {
  def header = headerOpt match {
    case Some(h) => h
    case None    => periodEntry.header
  }
  override def toString = "%s: %s" format (periodEntry, todo)
}
