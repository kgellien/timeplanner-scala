package de.gellien.timeplanner.timeplan

abstract sealed class SinglePeriod(val todo: List[ToDoList], val header: Option[String])

// TODO: collect format strings used for toString methods at one place!

case class Day(year: Int, month: Int, day: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-%02d-%02d: %s" format (year, month, day, todo)
}

case class Week(year: Int, week: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-W%02d: %s" format (year, week, todo)
}

case class Month(year: Int, month: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-%02d: %s" format (year, month, todo)
}

case class Quarter(year: Int, quarter: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-Q%d: %s" format (year, quarter, todo)
}

case class Year(year: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d: %s" format (year, todo)
}
