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
      val classifier = task match {
        case Task(_, Some(classifier), _, _) => classifier
        case _                               => miscHeader
      }
      append(taskMap, task, classifier, dummyTask)
    }
    val dummyAppointment = Appointment(YearlyEntry(), None, Nil, "09:00", "DummyAppointment")
    val appointmentMap = new HashMap[String, List[Appointment]]()
    for (appointment <- period.todo.appointments) {
      val classifier = appointment match {
        case Appointment(_, Some(classifier), _, _, _) => classifier
        case _                                         => miscHeader
      }
      append(appointmentMap, appointment, classifier, dummyAppointment)
    }
    val classifiers = (appointmentMap.keySet ++ taskMap.keySet + miscHeader).toList.sorted
    val lsp = for {
      classifier <- classifiers
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
