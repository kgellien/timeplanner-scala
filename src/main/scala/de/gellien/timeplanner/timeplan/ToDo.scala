package de.gellien.timeplanner.timeplan

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

abstract sealed class ToDo(val periodEntry: PeriodBase, val info: String)

case class Anniversary(
    override val periodEntry: PeriodBase,
    val yearOpt: Option[Int],
    override val info: String) extends ToDo(periodEntry, info) {
  def toLatex = yearOpt match {
    case Some(year) => "%s (%s)" format (info, year)
    case _          => info
  }
}

case class Appointment(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    val timeInfo: String,
    override val info: String) extends ToDo(periodEntry, info) {
  val timeInfoForLatex = timeInfo.replace(" - ", " -- ")
  def toLatex = "%s %s" format (timeInfoForLatex, info)
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s" format (classifier, toLatex)
    case _                => toLatex
  }
}

case class Task(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    override val info: String) extends ToDo(periodEntry, info) {
  def toLatex = info
  def toLatexWithClassifier = classifierOpt match {
    case Some(classifier) => "[%s] %s" format (classifier, info)
    case _                => info
  }
}

object Appointment {
  def extractSubTaskPeriodEntry(pe: PeriodEntry, a: Appointment): List[Task] = {
    // periodEntry here is known to be of type PeriodEntry
    val first = TimeHelper.getFirstDayOfPeriod(pe)
    val last = TimeHelper.getLastDayOfPeriod(pe)
    val (fromDay, fromMonth, toDay, toMonth) = try {
      val re = """(\d\d?)\.(\d\d?)?\.? --? (\d\d?)?\.?(\d\d?)?\.?""".r
      val re(fds, fms, tds, tms) = a.timeInfo
      val fd = if (fds == null) None else Some(fds.toInt)
      val fm = if (fms == null) None else Some(fms.toInt)
      val td = if (tds == null) None else Some(tds.toInt)
      val tm = if (tms == null) None else Some(tms.toInt)
      (fd, fm, td, tm)
    } catch {
      case me: scala.MatchError => {
        println("ERROR in extractSubTasks for %s" format this)
        (None, None, None, None)
      }
    }
    val result = a.periodEntry match {
      case pe @ WeekEntry(y, w) => {
        // TODO: check questionable logic! Just good enough for *current* uses
        val monthFirst = first.getMonthOfYear
        val yearFirst = first.getYear
        val monthLast = last.getMonthOfYear
        val yearLast = last.getYear
        val fromD = fromDay.getOrElse(0)
        val toD = toDay.getOrElse(-1)
        val fromM = fromMonth.getOrElse(monthFirst)
        val toM = toMonth.getOrElse(monthLast)
        val from = DayEntry(yearFirst, fromM, fromD)
        val to = DayEntry(yearLast, toM, toD)
        val lst = for {
          day <- TimeHelper.daysInPeriod(from, to)
        } yield Task(day, a.classifierOpt, a.dateBounds, a.info)
        lst
      }
      case _ => Nil
    }
    result
  }
  def extractSubTasks(a: Appointment): List[Task] = {
    a.periodEntry match {
      case pe: PeriodEntry => extractSubTaskPeriodEntry(pe, a)
      case _               => Nil
    }
  }
}
