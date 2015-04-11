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
    //    if (allClassifiers.size < 7) {
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
      period match {
        case Day(periodEntry, todo, header) =>
          val lsp = for {
            classifier <- allClassifiers
            appointments = appointmentMap.getOrElse(classifier, Nil)
            tasks = taskMap.getOrElse(classifier, Nil)
            toDoList = ToDoList(period.todo.anniversaries, appointments, tasks)
          } yield Day(periodEntry, toDoList, Some(classifier))
          lsp.toList
        case Week(periodEntry, todo, header) =>
          val lsp = for {
            classifier <- allClassifiers
            appointments = appointmentMap.getOrElse(classifier, Nil)
            tasks = taskMap.getOrElse(classifier, Nil)
            toDoList = ToDoList(period.todo.anniversaries, appointments, tasks)
          } yield Week(periodEntry, toDoList, Some(classifier))
          lsp.toList
        case Month(periodEntry, todo, header) =>
          val lsp = for {
            classifier <- allClassifiers
            appointments = appointmentMap.getOrElse(classifier, Nil)
            tasks = taskMap.getOrElse(classifier, Nil)
            toDoList = ToDoList(period.todo.anniversaries, appointments, tasks)
          } yield Month(periodEntry, toDoList, Some(classifier))
          lsp.toList
        case Quarter(periodEntry, todo, header) =>
          val lsp = for {
            classifier <- allClassifiers
            appointments = appointmentMap.getOrElse(classifier, Nil)
            tasks = taskMap.getOrElse(classifier, Nil)
            toDoList = ToDoList(period.todo.anniversaries, appointments, tasks)
          } yield Quarter(periodEntry, toDoList, Some(classifier))
          lsp.toList
        case Year(periodEntry, todo, header) =>
          val lsp = for {
            classifier <- allClassifiers
            appointments = appointmentMap.getOrElse(classifier, Nil)
            tasks = taskMap.getOrElse(classifier, Nil)
            toDoList = ToDoList(period.todo.anniversaries, appointments, tasks)
          } yield Year(periodEntry, toDoList, Some(classifier))
          lsp.toList
      }
    }
    //    } else {
    //      splitPeriodOld(period)
    //    }
  }

  //  def splitPeriodOld(period: SinglePeriod): List[SinglePeriod] = {
  //    // TODO: the following names should be configurable via localization / properties
  //    val taskHeaderNameA = if (Locale.getDefault.toString().startsWith("de")) "Privat" else "Private"
  //    val taskHeaderNameB = if (Locale.getDefault.toString().startsWith("de")) "Beruf" else "Business"
  //    val taskHeaderNameC = if (Locale.getDefault.toString().startsWith("de")) "Sonstiges" else "Miscellaneous"
  //    val taskHeaderClassifierA = "P"
  //    val taskHeaderClassifierB = "B"
  //    val (todoA, rest) = splitToDoLists(taskHeaderClassifierA, period.todo)
  //    val (todoB, todoC) = splitToDoLists(taskHeaderClassifierB, rest)
  //    period match {
  //      case Day(periodEntry, todo, header) => // to be exhaustive; not used yet
  //        List(Day(periodEntry, todoA, Some(taskHeaderNameA)), Day(periodEntry, todoC, Some(taskHeaderNameC)), Day(periodEntry, todoB, Some(taskHeaderNameB)))
  //      case Week(periodEntry, todo, header) =>
  //        List(Week(periodEntry, todoA, Some(taskHeaderNameA)), Week(periodEntry, todoC, Some(taskHeaderNameC)), Week(periodEntry, todoB, Some(taskHeaderNameB)))
  //      case Month(periodEntry, todo, header) =>
  //        List(Month(periodEntry, todoA, Some(taskHeaderNameA)), Month(periodEntry, todoC, Some(taskHeaderNameC)), Month(periodEntry, todoB, Some(taskHeaderNameB)))
  //      case Quarter(periodEntry, todo, header) =>
  //        List(Quarter(periodEntry, todoA, Some(taskHeaderNameA)), Quarter(periodEntry, todoC, Some(taskHeaderNameC)), Quarter(periodEntry, todoB, Some(taskHeaderNameB)))
  //      case Year(periodEntry, todo, header) =>
  //        List(Year(periodEntry, todoA, Some(taskHeaderNameA)), Year(periodEntry, todoC, Some(taskHeaderNameC)), Year(periodEntry, todoB, Some(taskHeaderNameB)))
  //    }
  //  }
  //
  //  def splitToDoLists(classifier: String, todo: ToDoList): (ToDoList, ToDoList) = {
  //    val (appointmentsWithClassifier, appointmentsWithoutClassifier) = splitOnClassifier(classifier, todo.appointments)
  //    val (tasksWithClassifier, tasksWithoutClassifier) = splitOnClassifier(classifier, todo.tasks)
  //    (ToDoList(todo.anniversaries, appointmentsWithClassifier.map { _.asInstanceOf[Appointment] }, tasksWithClassifier.map { _.asInstanceOf[Task] }), ToDoList(todo.anniversaries, appointmentsWithoutClassifier.map { _.asInstanceOf[Appointment] }, tasksWithoutClassifier.map { _.asInstanceOf[Task] }))
  //  }
  //
  //  def splitOnClassifier(classifier: String, list: List[ToDo]): (List[ToDo], List[ToDo]) = {
  //    val withClassifier = new ListBuffer[ToDo]
  //    val withoutClassifier = new ListBuffer[ToDo]
  //    for (todo <- list) {
  //      todo match {
  //        case Appointment(_, Some(`classifier`), _, _, _) => withClassifier += todo
  //        case Appointment(_, _, _, _, _) => withoutClassifier += todo
  //        case Task(_, Some(`classifier`), _, _) => withClassifier += todo
  //        case Task(_, _, _, _) => withoutClassifier += todo
  //        case _ => ;
  //      }
  //    }
  //    (withClassifier.toList, withoutClassifier.toList)
  //  }
}
