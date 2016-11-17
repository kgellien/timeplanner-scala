package de.gellien.timeplanner.timeplan

case class ToDoList(val anniversaries: List[Anniversary], val appointments: List[Appointment], val tasks: List[Task])

abstract sealed class ToDo(val periodEntry: PeriodBase, val info: String)

case class Anniversary(
    override val periodEntry: PeriodBase,
    val yearOpt: Option[Int],
    override val info: String) extends ToDo(periodEntry, info) {
}

case class Appointment(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    val timeInfo: String,
    override val info: String) extends ToDo(periodEntry, info) {
}

case class Task(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    override val info: String) extends ToDo(periodEntry, info) {
}

object Appointment {
  def extractSubTaskPeriodEntry(pe: PeriodEntry, a: Appointment) = {
//  	println(a)
    // periodEntry here is known to be of type PeriodEntry
    val first = TimeHelper.getFirstDayOfPeriod(pe)
    val last = TimeHelper.getLastDayOfPeriod(pe)
    val (fromDay, fromMonth, fromYear, toDay, toMonth, toYear) = try {
      val re = """(\d\d?)\.((\d\d?)\.)?(\d\d\d\d)?( - (\d\d?)\.(\d\d?)\.(\d\d\d\d)?)?""".r
      val re(fds, _, fms, fys, to, tds, tms, tys) = a.timeInfo
      val fd = if (fds == null) None else Some(fds.toInt)
      val fm = if (fms == null) None else Some(fms.toInt)
      val fy = if (fys == null) None else Some(fys.toInt)
      val td = if (tds == null) None else Some(tds.toInt)
      val tm = if (tms == null) None else Some(tms.toInt)
      val ty = if (tys == null) None else Some(tys.toInt)
      (fd, fm, fy, td, tm, ty)
    } catch {
      case me: scala.MatchError => {
        println("ERROR in extractSubTasks for %s" format this)
        (None, None, None, None, None, None)
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
        val fromM = fromMonth.getOrElse(monthFirst)
        val from = DayEntry(yearFirst, fromM, fromD)
        val to = toDay match {
        	case Some(toD) =>
              val toM = toMonth.getOrElse(monthLast)
        	  DayEntry(yearLast, toM, toD)
        	case None     => from
        }
        val lst = for {
          day <- TimeHelper.daysInPeriod(from, to)
//        } yield Appointment(day, a.classifierOpt, a.dateBounds, a.timeInfo, a.info)
        } yield Appointment(day, a.classifierOpt, a.dateBounds, "xx:xx", a.info)
//        } yield Task(day, a.classifierOpt, a.dateBounds, a.info)
//        for (task <- lst) println("  " + task)
        lst
      }
      case _ => Nil
    }
    result
  }
  def extractSubTasks(a: Appointment) = {
    a.periodEntry match {
      case pe: PeriodEntry => extractSubTaskPeriodEntry(pe, a)
      case _               => Nil
    }
  }
}
