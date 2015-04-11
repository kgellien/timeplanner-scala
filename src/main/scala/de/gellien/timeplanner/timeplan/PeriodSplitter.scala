package de.gellien.timeplanner.timeplan

import java.util.Locale
import scala.collection.mutable.{ ListBuffer, HashMap, Map }

object PeriodSplitter {

  def splitPeriod(period: SinglePeriod): List[SinglePeriod] = {
    def append[T](theMap: Map[String, List[T]], entry: T, classifier: String, dummy: T) = {
      if (theMap.isDefinedAt(classifier))
        theMap += (classifier -> theMap.getOrElse(classifier, List(dummy)))
      else theMap += (classifier -> List(entry))
    }
    val miscHeader = if (Locale.getDefault.toString().startsWith("de")) "Sonstiges" else "Miscellaneous"
    val dummyTask = Task(YearlyEntry(), None, Nil, "DummyTask")
    val taskMap = new HashMap[String, List[Task]]()
    for (task <- period.todo.tasks) {
      task match {
        case Task(_, Some(classifier), _, _) =>
          append(taskMap, task, classifier, dummyTask)
        case _ =>
          val classifier = miscHeader
          append(taskMap, task, classifier, dummyTask)
      }
    }
    val dummyAppointment = Appointment(YearlyEntry(), None, Nil, "09:00", "DummyAppointment")
    val appointmentMap = new HashMap[String, List[Appointment]]()
    for (appointment <- period.todo.appointments) {
      appointment match {
        case Appointment(_, Some(classifier), _, _, _) =>
          append(appointmentMap, appointment, classifier, dummyAppointment)
        case _ =>
          val classifier = miscHeader
          append(appointmentMap, appointment, classifier, dummyAppointment)
      }
    }
    val allClassifiers = (appointmentMap.keySet ++ taskMap.keySet).toList.sorted
    if (allClassifiers.isEmpty) {
      period match {
        case Day(periodEntry, todo, header) =>
          List(Day(periodEntry, todo, Some(miscHeader)))
        case Week(periodEntry, todo, header) =>
          List(Week(periodEntry, todo, Some(miscHeader)))
        case Month(periodEntry, todo, header) =>
          List(Month(periodEntry, todo, Some(miscHeader)))
        case Quarter(periodEntry, todo, header) =>
          List(Quarter(periodEntry, todo, Some(miscHeader)))
        case Year(periodEntry, todo, header) =>
          List(Year(periodEntry, todo, Some(miscHeader)))
      }
    } else {
      val lsp = for {
        classifier <- allClassifiers
        appointments = appointmentMap.getOrElse(classifier, Nil)
        tasks = taskMap.getOrElse(classifier, Nil)
        toDoList = ToDoList(period.todo.anniversaries, appointments, tasks)
      } yield {
        period match {
          case Day(periodEntry, todo, header)     => Day(periodEntry, toDoList, Some(classifier))
          case Week(periodEntry, todo, header)    => Week(periodEntry, toDoList, Some(classifier))
          case Month(periodEntry, todo, header)   => Month(periodEntry, toDoList, Some(classifier))
          case Quarter(periodEntry, todo, header) => Quarter(periodEntry, toDoList, Some(classifier))
          case Year(periodEntry, todo, header)    => Year(periodEntry, toDoList, Some(classifier))
        }
      }
      lsp.toList
    }
  }
}
