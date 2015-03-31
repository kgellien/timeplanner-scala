package de.gellien.timeplanner.timeplan

import scala.collection.mutable.ListBuffer

object PeriodSplitter {

  // TODO: the following names should be configurable via localization / properties
  val taskHeaderNameA = "Privat"
  val taskHeaderNameB = "Beruflich"
  val taskHeaderNameC = "Sonstiges"
  val taskHeaderClassifierA = "P"
  val taskHeaderClassifierB = "B"

//  def startsWithTaskHeaderClassifier(entry: String) = entry.startsWith(taskHeaderClassifierA) || entry.startsWith(taskHeaderClassifierB)

  def splitPeriod(period: SinglePeriod): List[SinglePeriod] = {
    val (todoA, rest) = splitToDoLists(taskHeaderClassifierA, period.todo)
    val (todoB, todoC) = splitToDoLists(taskHeaderClassifierB, rest)
    period match {
      case Day(year, month, day, todo, header) => // to be exhaustive; not used yet
        List(Day(year, month, day, todoA, Some(taskHeaderNameA)), Day(year, month, day, todoC, Some(taskHeaderNameC)), Day(year, month, day, todoB, Some(taskHeaderNameB)))
      case Week(year, week, todo, header) =>
        List(Week(year, week, todoA, Some(taskHeaderNameA)), Week(year, week, todoC, Some(taskHeaderNameC)), Week(year, week, todoB, Some(taskHeaderNameB)))
      case Month(year, month, todo, header) =>
        List(Month(year, month, todoA, Some(taskHeaderNameA)), Month(year, month, todoC, Some(taskHeaderNameC)), Month(year, month, todoB, Some(taskHeaderNameB)))
      case Quarter(year, quarter, todo, header) =>
        List(Quarter(year, quarter, todoA, Some(taskHeaderNameA)), Quarter(year, quarter, todoC, Some(taskHeaderNameC)), Quarter(year, quarter, todoB, Some(taskHeaderNameB)))
      case Year(year, todo, header) =>
        List(Year(year, todoA, Some(taskHeaderNameA)), Year(year, todoC, Some(taskHeaderNameC)), Year(year, todoB, Some(taskHeaderNameB)))
    }
  }

  def splitToDoLists(classifier: String, todo: ToDoList): (ToDoList, ToDoList) = {
    val (appointmentsWithPrefix, appointmentsWithoutPrefix) = splitOnClassifier(classifier, todo.appointments)
    val (tasksWithPrefix, tasksWithoutPrefix) = splitOnClassifier(classifier, todo.tasks)
    (ToDoList(todo.anniversaries, appointmentsWithPrefix.map { _.asInstanceOf[Appointment] }, tasksWithPrefix.map { _.asInstanceOf[Task] }), ToDoList(todo.anniversaries, appointmentsWithoutPrefix.map { _.asInstanceOf[Appointment] }, tasksWithoutPrefix.map { _.asInstanceOf[Task] }))
  }

  def splitOnClassifier(classifier: String, list: List[ToDoEntry]): (List[ToDoEntry], List[ToDoEntry]) = {
    val withClassifier = new ListBuffer[ToDoEntry]
    val withoutClassifier = new ListBuffer[ToDoEntry]
    for (todo <- list) {
      todo match {
        case Appointment(_, Some(`classifier`), _, _) => withClassifier += todo
        case Appointment(_, _, _, _)                  => withoutClassifier += todo
        case Task(_, Some(`classifier`), _)           => withClassifier += todo
        case Task(_, _, _)                            => withoutClassifier += todo
        case _                                        => ;
      }
    }
    (withClassifier.toList, withoutClassifier.toList)
  }
}
