package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

abstract sealed trait DateTime extends Ordered[DateTime] {
  def short = toString
}
// TODO: improve Ordered / compare! for now just good enough that it type-checks
case class Date(year: Int, month: Int, day: Int) extends DateTime {
  def compare(that: DateTime) = that match {
    case d: Date => asIso.compare(d.asIso)
    case t: Time => asIso.compare(t.toString) // should not be possibleÏ
  }
  override def toString = f"${day}%02d.${month}%02d.${year}%04d"
  def asIso = f"${year}%04d-${month}%02d-${day}%02d"
  override def short = if (month == 0) f"${day}%02d."
  else if (year == 0) f"${day}%02d.${month}%02d."
  else toString
}
case class Time(hh: Int, mm: Int) extends DateTime {
  def compare(that: DateTime) = toString.compare(that.toString)
  override def toString = f"${hh}%02d:${mm}%02d"
}

// TODO: how do I guarantee that from and to are either both of type Date or of type Time?
case class Range(from: DateTime, to: DateTime) extends Ordered[Range] {
  def compare(that: Range) = from.compare(that.from)
}

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
    val timeInfo: Range,
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
    val result = a.periodEntry match {
      case pe @ WeekEntry(y, w) => {
        def date2DayEntry(date: Date, local: LocalDate) = {
          // TODO: further logic!
          // check with ToDoHelper.extractTimeInfo
          val year = if (date.year == 0) local.getYear else date.year
          val month = if (date.month == 0) local.getMonthOfYear else date.month
          DayEntry(year, month, date.day)
        }
        val (from, to) = a.timeInfo match {
          case Range(f: Date, t: Date) => (date2DayEntry(f, first), date2DayEntry(t, last))
        }
        val lst = for {
          day <- TimeHelper.daysInPeriod(from, to)
        } yield Appointment(day, a.classifierOpt, a.dateBounds, Range(Time(0, 0), Time(0, 0)), a.info)
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
