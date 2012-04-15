package de.gellien.timeplanner.timeplan

object PeriodSplitter {

  // TODO: the following names should be configurable via localization
  val taskHeaderNameA = "Privat"
  val taskHeaderNameB = "Beruflich"
  val taskHeaderNameC = "Sonstiges"
    // Attention: taskHeader prefixes need to be of same length!
  val taskHeaderPrefixA = "P "
  val taskHeaderPrefixB = "B "
  def taskHeaderPrefixSize = taskHeaderPrefixA.size

  def startsWithTaskHeaderPrefix(entry: String) = entry.startsWith(taskHeaderPrefixA) || entry.startsWith(taskHeaderPrefixB)
   
  def splitPeriod(period: SinglePeriod): List[SinglePeriod] = {
    val (todoA, rest) = splitToDoLists(taskHeaderPrefixA, period.todo)
    val (todoB, todoC) = splitToDoLists(taskHeaderPrefixB, rest)
    val (col1, col2, col3) = period match {
      case Day(year, month, day, todo, header) => // to be exhaustive; not used yet
        (Day(year, month, day, todoA, Some(taskHeaderNameA)), Day(year, month, day, todoC, Some(taskHeaderNameC)), Day(year, month, day, todoB, Some(taskHeaderNameB)))
      case Week(year, week, todo, header) =>
        (Week(year, week, todoA, Some(taskHeaderNameA)), Week(year, week, todoC, Some(taskHeaderNameC)), Week(year, week, todoB, Some(taskHeaderNameB)))
      case Month(year, month, todo, header) =>
        (Month(year, month, todoA, Some(taskHeaderNameA)), Month(year, month, todoC, Some(taskHeaderNameC)), Month(year, month, todoB, Some(taskHeaderNameB)))
      case Quarter(year, quarter, todo, header) =>
        (Quarter(year, quarter, todoA, Some(taskHeaderNameA)), Quarter(year, quarter, todoC, Some(taskHeaderNameC)), Quarter(year, quarter, todoB, Some(taskHeaderNameB)))
      case Year(year, todo, header) =>
        (Year(year, todoA, Some(taskHeaderNameA)), Year(year, todoC, Some(taskHeaderNameC)), Year(year, todoB, Some(taskHeaderNameB)))
    }
    val result = List(col1, col2, col3)
   result
  }

  def splitToDoLists(prefix: String, todoLists: List[ToDoList]): (List[ToDoList], List[ToDoList]) = {
    val preliminaryResult = for (todo <- todoLists)
      yield splitOnPrefix(prefix, todo)
    val withPrefix = for (pair <- preliminaryResult) yield pair._1
    val withoutPrefix = for (pair <- preliminaryResult) yield pair._2
    (withPrefix, withoutPrefix)
  }

  def splitOnPrefix(prefix: String, todoList: ToDoList): (ToDoList, ToDoList) = {
    val withPrefix = for (entry <- todoList.todo if entry startsWith prefix) yield entry.replaceFirst(prefix, "")
    val withoutPrefix = for (entry <- todoList.todo if !(entry startsWith prefix)) yield entry
    (new ToDoList(todoList.kind, withPrefix), new ToDoList(todoList.kind, withoutPrefix))
  }
}
