package de.gellien.timeplanner.timeplan

abstract sealed class ToDoListType

case object Anniversary extends ToDoListType
case object Task extends ToDoListType
case object Appointment extends ToDoListType


case class ToDoList(val kind: ToDoListType, val todo: List[String]) {
  def isEmpty = todo.isEmpty
  override def toString = "%s: %s" format (kind, todo)
}
