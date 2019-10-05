package de.gellien.timeplanner.timeplan

import java.time.format.TextStyle
import java.time.{DayOfWeek, LocalDate, LocalDateTime}
import java.time.temporal.WeekFields
import java.util.{Calendar, Locale}

import scala.collection.mutable.ListBuffer

sealed case class IsoDate(year: Int, month: Int, day: Int) extends Ordered[IsoDate] {
  override def toString = "%04d-%02d-%02d" format (year, month, day)
  def compare(that: IsoDate) = toString.compare(that.toString)
}

object TimeHelper {
  val weekFields = WeekFields.ISO

  def getWeekyear(date: LocalDate) = {
    // MONDAY as start of week!
    def addOne(date: LocalDate) = {
      // TODO: check for leap year
      val last = LocalDate.of(date.getYear, 12, 31)
      val dayOfYear = date.getDayOfYear
      last.getDayOfWeek match {
        case DayOfWeek.MONDAY => dayOfYear >= 365
        case DayOfWeek.TUESDAY => dayOfYear >= 364
        case DayOfWeek.WEDNESDAY => dayOfYear >= 365
        case _ => false
      }
    }
    def subOne(date: LocalDate) = {
      val first = LocalDate.of(date.getYear, 1, 1)
      val dayOfYear = date.getDayOfYear
      first.getDayOfWeek match {
      case DayOfWeek.FRIDAY => dayOfYear <= 3
      case DayOfWeek.SATURDAY => dayOfYear <= 2
      case DayOfWeek.SUNDAY => dayOfYear <= 1
      case _ => false
      }
    }
    //date.getWeekyear // Joda: return year of week (relevant for first/last week in year
    // TODO: simplify / check for library function
    //if (date.getDayOfYear > 362 && date.getDayOfWeek.getValue < DayOfWeek.THURSDAY.getValue) {
    if (addOne(date)) {
      date.getYear + 1
    } else if (subOne(date)) {
      date.getYear - 1
    } else {
      date.getYear
    }
  }

  def getDayOfWeek(periodEntry: DayEntry): Int = {
    val currentDay = LocalDate.of(periodEntry.year, periodEntry.month, periodEntry.day)
    currentDay.getDayOfWeek.getValue
  }

  def isoDate(date: LocalDate): IsoDate =
    IsoDate(date.getYear, date.getMonthValue, date.getDayOfMonth)

  def monthName(month: Int) = // year and day do not matter
    LocalDate.of(2004, month, 1).getMonth.getDisplayName(TextStyle.FULL , Locale.getDefault)

  def displayDay(day: LocalDate): String =
    "%s, %02d.%02d." format (day.getDayOfWeek.getDisplayName(TextStyle.FULL , Locale.getDefault), day.getDayOfMonth, day.getMonthValue)

  def displayDay(year: Int, month: Int, day: Int): String =
    displayDay(LocalDate.of(year, month, day))

  def getFirstDayInWeek(year: Int, isoWeek: Int): LocalDate = {
    val calendar = Calendar.getInstance()
    calendar.clear()
    calendar.set(Calendar.WEEK_OF_YEAR, isoWeek)
    calendar.set(Calendar.YEAR, year)
    LocalDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId()).toLocalDate();
  }

  def daysInPeriod(startDay: LocalDate, endDay: LocalDate): List[LocalDate] = {
    val result = new ListBuffer[LocalDate]
    var currentDay = startDay
    while (currentDay.compareTo(endDay) <= 0) {
      result += currentDay
      currentDay = currentDay plusDays 1
    }
    result.toList
  }

  def dayEntry2LocalDate(dayEntry: DayEntry): LocalDate =
    LocalDate.of(dayEntry.year, dayEntry.month, dayEntry.day)

  def localDate2DayEntry(date: LocalDate): DayEntry =
    DayEntry(date.getYear, date.getMonthValue, date.getDayOfMonth)

  def daysInPeriod(startDay: DayEntry, endDay: DayEntry): List[DayEntry] =
    for {
      day <- daysInPeriod(dayEntry2LocalDate(startDay), dayEntry2LocalDate(endDay))
    } yield localDate2DayEntry(day)

  def daysInWeek(year: Int, week: Int): List[LocalDate] = {
    val startDay = getFirstDayInWeek(year, week)
    daysInPeriod(startDay, startDay plusDays 6)
  }

  def weeksInYear(year: Int): Int = {
    val date = LocalDate.of(year, 12, 31)
    if (date.get(weekFields.weekOfWeekBasedYear()) == 1) (date minusWeeks 1).get(weekFields.weekOfWeekBasedYear())
    else date.get(weekFields.weekOfWeekBasedYear())
  }

  def weeksInQuarter(year: Int, quarter: Int): List[(Int, Int)] =
    (for (month <- monthsInQuarter(year, quarter))
      yield weeksInMonth(year, month)).flatten

  def weeksInMonth(year: Int, month: Int): List[(Int, Int)] = {
    val firstOfMonth = LocalDate.of(year, month, 1)
    val lastOfMonth = if (month == 12)
      LocalDate.of(year, 12, 31)
    else
      (LocalDate.of(year, month + 1, 1)) minusDays 1
    val startWeek = (getWeekyear(firstOfMonth), firstOfMonth.get(weekFields.weekOfWeekBasedYear()))
    val endWeek = (getWeekyear(lastOfMonth), lastOfMonth.get(weekFields.weekOfWeekBasedYear()))
    val firstMonday = getFirstDayInWeek(startWeek._1, startWeek._2)
    val noOfWeeks =
      if (startWeek._1 == endWeek._1) endWeek._2 - startWeek._2 + 1
      else if (month == 1) endWeek._2 + 1
      else if (month == 12) (weeksInYear(year) + 1) - startWeek._2 + 1
      else 0 // dummy; must not happen!
    val result = (for {
      i <- 0 until noOfWeeks
      currentMonday = firstMonday.plusWeeks(i)
    } yield (getWeekyear(currentMonday), currentMonday.get(weekFields.weekOfWeekBasedYear()))).toList
    result
  }

  def fromToWeek(year: Int, week: Int): (String, String) = {
    val firstDay = getFirstDayInWeek(year, week)
    val lastDay = firstDay plusDays 6
    val start = ("%02d." format firstDay.getDayOfMonth) +
      (if (firstDay.getMonthValue != lastDay.getMonthValue) ("%02d." format firstDay.getMonthValue)
      else "") +
      (if (firstDay.getYear != lastDay.getYear) ("%d" format firstDay.getYear)
      else "")
    val end = "%02d.%02d.%d" format (lastDay.getDayOfMonth, lastDay.getMonthValue, lastDay.getYear)
    (start, end)
  }

  def monthsInQuarter(year: Int, quarter: Int): List[Int] = {
    val start = (quarter - 1) * 3 + 1
    (start to (start + 2)).toList
  }

  def getFirstDayOfPeriod(pe: PeriodEntry): LocalDate = {
    pe match {
      case YearEntry(year)               => LocalDate.of(year, 1, 1)
      case QuarterEntry(year, quarter)   => LocalDate.of(year, TimeHelper.monthsInQuarter(year, quarter).head, 1)
      case MonthEntry(year, month)       => LocalDate.of(year, month, 1)
      case WeekEntry(year, week)         => TimeHelper.getFirstDayInWeek(year, week)
      case DayEntry(year, month, day, _) => LocalDate.of(year, month, day)
    }
  }

  def getLastDayOfPeriod(pe: PeriodEntry): LocalDate = {
    pe match {
      case YearEntry(year) => LocalDate.of(year, 12, 31)
      case QuarterEntry(year, quarter) =>
        val ld = LocalDate.of(year, TimeHelper.monthsInQuarter(year, quarter).reverse.head, 1)
        ld plusMonths (1) minusDays (1)
      case MonthEntry(year, month) =>
        val ld = LocalDate.of(year, month, 1)
        ld plusMonths (1) minusDays (1)
      case WeekEntry(year, week)         => TimeHelper.getFirstDayInWeek(year, week) plusDays (6)
      case DayEntry(year, month, day, _) => LocalDate.of(year, month, day)
    }
  }
}
