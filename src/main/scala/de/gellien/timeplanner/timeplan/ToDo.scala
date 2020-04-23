package de.gellien.timeplanner.timeplan

import java.time.LocalDate

abstract sealed trait DateTime extends Ordered[DateTime] {
  def short = toString
}
// TODO: improve Ordered / compare! for now just good enough that it type-checks
case class Date(
    year: Int,
    month: Int,
    day: Int) extends DateTime {
  def compare(that: DateTime) = that match {
    case d: Date => asIsoString.compare(d.asIsoString)
    case t: Time => asIsoString.compare(t.toString) // should not be possibleÏ
  }
  override def toString = f"${day}%02d.${month}%02d.${year}%04d"
  def asIsoString = f"${year}%04d-${month}%02d-${day}%02d"
  override def short =
    if (month == 0) f"${day}%02d."
    else if (year == 0) f"${day}%02d.${month}%02d."
    else toString
}
case class Time(
    hh: Int,
    mm: Int) extends DateTime {
  def compare(that: DateTime) = toString.compare(that.toString)
  override def toString = f"${hh}%02d:${mm}%02d"
}

object DateTimeHelper {
  def timeDiff(from: Time, to: Time) = {
    //Pre: from < to and both on the same day
    val toT = to.hh * 60 + to.mm
    val fromT = from.hh * 60 + from.mm
    (toT - fromT) / 60.0
  }
}

// TODO: how do I guarantee that from and to are either both of type Date or of type Time?
case class Range(
    from: DateTime,
    to: DateTime) extends Ordered[Range] {
  def compare(that: Range) = from.compare(that.from)
}

case class ToDoList(
  val specials: List[Special],
  val anniversaries: List[Anniversary],
  val appointments: List[Appointment],
  val tasks: List[Task])

abstract sealed class ToDo(
  val periodEntry: PeriodBase,
  val info: String)

case class Special(
  override val periodEntry: PeriodBase,
  override val info: String) extends ToDo(periodEntry, info)

case class Anniversary(
  override val periodEntry: PeriodBase,
  val yearOpt: Option[Int],
  override val info: String) extends ToDo(periodEntry, info)

case class Appointment(
    override val periodEntry: PeriodBase,
    val classifierOpt: Option[String],
    val dateBounds: List[DateBound],
    val timeInfo: Range,
    override val info: String) extends ToDo(periodEntry, info) {
  val timeInfoSort = periodEntry match {
    case pe: PeriodEntry => timeInfo.from match {
      // TODO improve incomplete/questionable logic
      //      case Date(0, 0, day)     => Date(pe.lower.year, pe.lower.month, day).asIso
      case Date(0, 0, day) =>
        val monthTo = timeInfo.to.asInstanceOf[Date].month // must be of right type
        // currently monthTo must be != 0; here just to be safe
        val month = if (monthTo != 0) monthTo else pe.lower.month
        Date(pe.lower.year, month, day).asIsoString
      case Date(0, month, day)    => Date(pe.lower.year, month, day).asIsoString
      case Date(year, month, day) => Date(year, month, day).asIsoString
      case from: Time             => from.toString
    }
    case pb: PeriodBase => timeInfo.from.toString
  }
}

case class Task(
  override val periodEntry: PeriodBase,
  val classifierOpt: Option[String],
  val dateBounds: List[DateBound],
  override val info: String) extends ToDo(periodEntry, info)

object Appointment {
  def extractSubTaskPeriodEntry(pe: PeriodEntry, a: Appointment) = {
    val first = TimeHelper.getFirstDayOfPeriod(pe)
    val last = TimeHelper.getLastDayOfPeriod(pe)
    val result = a.periodEntry match {
      case pe@WeekEntry(y, w) => {
        def date2DayEntry(date: Date, local: LocalDate) = {
          // TODO: further logic!
          val year = if (date.year == 0) local.getYear else date.year
          val month = if (date.month == 0) local.getMonthValue else date.month
          DayEntry(year, month, date.day)
        }
        val (from, to) = a.timeInfo match {
          case Range(f: Date, t: Date) =>
            (date2DayEntry(f, first), date2DayEntry(t, last))
          case _                       => ???
        }
        val zeroTimeRange = Range(Time(0, 0), Time(0, 0))
        for {
          day <- TimeHelper.daysInPeriod(from, to)
        } yield a.copy(periodEntry = day, timeInfo = zeroTimeRange)
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
