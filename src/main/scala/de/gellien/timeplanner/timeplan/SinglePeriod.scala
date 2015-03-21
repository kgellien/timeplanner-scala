package de.gellien.timeplanner.timeplan

abstract sealed class SinglePeriod(val todo: List[ToDoList], val header: Option[String])

// TODO: collect format strings used for toString methods at one place!

case class Day(year: Int, month: Int, day: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-%02d-%02d" format (year, month, day)
}

case class Week(year: Int, week: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-W%02d" format (year, week)
}

case class Month(year: Int, month: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-%02d" format (year, month)
}

case class Quarter(year: Int, quarter: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d-Q%d" format (year, quarter)
}

case class Year(year: Int, override val todo: List[ToDoList],
    override val header: Option[String] = None) extends SinglePeriod(todo, header) {
  override def toString = "%4d" format (year)
}
