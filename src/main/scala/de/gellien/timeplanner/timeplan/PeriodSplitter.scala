package de.gellien.timeplanner.timeplan

import java.util.Locale
import scala.collection.mutable.ListBuffer

object PeriodSplitter {

  // TODO: the following names should be configurable via localization / properties
  val taskHeaderNameA = if (Locale.getDefault.toString().startsWith("de")) "Privat" else "Private"
  val taskHeaderNameB = if (Locale.getDefault.toString().startsWith("de")) "Beruf" else "Business"
  val taskHeaderNameC = if (Locale.getDefault.toString().startsWith("de")) "Sonstiges" else "Miscellaneous"
  val taskHeaderClassifierA = "P"
  val taskHeaderClassifierB = "B"

  def splitPeriod(period: SinglePeriod): List[SinglePeriod] = {
    val (todoA, rest) = splitToDoLists(taskHeaderClassifierA, period.todo)
    val (todoB, todoC) = splitToDoLists(taskHeaderClassifierB, rest)
    period match {
      case Day(periodEntry, todo, header) => // to be exhaustive; not used yet
        List(Day(periodEntry, todoA, Some(taskHeaderNameA)), Day(periodEntry, todoC, Some(taskHeaderNameC)), Day(periodEntry, todoB, Some(taskHeaderNameB)))
      case Week(periodEntry, todo, header) =>
        List(Week(periodEntry, todoA, Some(taskHeaderNameA)), Week(periodEntry, todoC, Some(taskHeaderNameC)), Week(periodEntry, todoB, Some(taskHeaderNameB)))
      case Month(periodEntry, todo, header) =>
        List(Month(periodEntry, todoA, Some(taskHeaderNameA)), Month(periodEntry, todoC, Some(taskHeaderNameC)), Month(periodEntry, todoB, Some(taskHeaderNameB)))
      case Quarter(periodEntry, todo, header) =>
        List(Quarter(periodEntry, todoA, Some(taskHeaderNameA)), Quarter(periodEntry, todoC, Some(taskHeaderNameC)), Quarter(periodEntry, todoB, Some(taskHeaderNameB)))
      case Year(year, todo, header) =>
        List(Year(year, todoA, Some(taskHeaderNameA)), Year(year, todoC, Some(taskHeaderNameC)), Year(year, todoB, Some(taskHeaderNameB)))
    }
  }

  def splitToDoLists(classifier: String, todo: ToDoList): (ToDoList, ToDoList) = {
    val (appointmentsWithPrefix, appointmentsWithoutPrefix) = splitOnClassifier(classifier, todo.appointments)
    val (tasksWithPrefix, tasksWithoutPrefix) = splitOnClassifier(classifier, todo.tasks)
    (ToDoList(todo.anniversaries, appointmentsWithPrefix.map { _.asInstanceOf[Appointment] }, tasksWithPrefix.map { _.asInstanceOf[Task] }), ToDoList(todo.anniversaries, appointmentsWithoutPrefix.map { _.asInstanceOf[Appointment] }, tasksWithoutPrefix.map { _.asInstanceOf[Task] }))
  }

  def splitOnClassifier(classifier: String, list: List[ToDo]): (List[ToDo], List[ToDo]) = {
    val withClassifier = new ListBuffer[ToDo]
    val withoutClassifier = new ListBuffer[ToDo]
    for (todo <- list) {
      todo match {
        case Appointment(_, Some(`classifier`), _, _) => withClassifier += todo
        case Appointment(_, _, _, _)                  => withoutClassifier += todo
        case Task(_, Some(`classifier`), _, _)        => withClassifier += todo
        case Task(_, _, _, _)                         => withoutClassifier += todo
        case _                                        => ;
      }
    }
    (withClassifier.toList, withoutClassifier.toList)
  }
}
