package de.gellien.timeplanner.timeplan

import java.util.Calendar
import org.joda.time.LocalDate

object TimeHelper {
  def getDayOfWeek(periodEntry: DayEntry): Int = {
    val currentDay = new LocalDate(periodEntry.year, periodEntry.month, periodEntry.day)
    currentDay.getDayOfWeek
  }
  
  def isoDate(date: LocalDate): String = {
    "%4d-%02d-%02d" format (date.getYear, date.getMonthOfYear, date.getDayOfMonth)
  }

  def monthName(month: Int) = { // year and day do not matter
    new LocalDate(2004, month, 1).monthOfYear.getAsText
  }

  def displayDay(day: LocalDate) = {
    "%s, %02d.%02d." format (day.dayOfWeek.getAsText, day.getDayOfMonth, day.getMonthOfYear)
  }

  def displayDay(year: Int, month: Int, day: Int) = {
    val date = new LocalDate(year, month, day)
    "%s, %02d.%02d." format (date.dayOfWeek.getAsText, date.getDayOfMonth, date.getMonthOfYear)
  }

  def getFirstDayInWeek(year: Int, isoWeek: Int): LocalDate = {
    val calendar = Calendar.getInstance()
    calendar.clear()
    calendar.set(Calendar.WEEK_OF_YEAR, isoWeek)
    calendar.set(Calendar.YEAR, year)
    new LocalDate(calendar)
  }

  def daysInWeek(year: Int, week: Int): List[LocalDate] = {
    val startDay = getFirstDayInWeek(year, week)
    (for {
      i <- 0 until 7
      currentDay = startDay plusDays i
    } yield currentDay).toList
  }

  def weeksInYear(year: Int): Int = {
    val date = new LocalDate(year, 12, 31)
    val result =
      if (date.getWeekOfWeekyear == 1) (date minusWeeks 1).getWeekOfWeekyear
      else date.getWeekOfWeekyear
    result
  }
  
  def weeksInQuarter(year: Int, quarter: Int): List[(Int, Int)] = {
    val miq = monthsInQuarter(year, quarter)
    (for (month <- miq)
      yield weeksInMonth(year, month)).flatten
  }

  def weeksInMonth(year: Int, month: Int): List[(Int, Int)] = {
    val firstOfMonth = new LocalDate(year, month, 1)
    val lastOfMonth = if (month == 12)
      new LocalDate(year, 12, 31)
    else
      (new LocalDate(year, month + 1, 1)) minusDays 1
    val startWeek = (firstOfMonth.getWeekyear, firstOfMonth.getWeekOfWeekyear)
    val endWeek = (lastOfMonth.getWeekyear, lastOfMonth.getWeekOfWeekyear)
    val firstMonday = getFirstDayInWeek(startWeek._1, startWeek._2)
    val noOfWeeks =
      if (startWeek._1 == endWeek._1) endWeek._2 - startWeek._2 + 1
      else if (month == 1) endWeek._2 + 1
      else if (month == 12) (weeksInYear(year) + 1) - startWeek._2 + 1
      else 0 // dummy; must not happen!
    val result = (for {
      i <- 0 until noOfWeeks
      currentMonday = firstMonday.plusWeeks(i)
    } yield (currentMonday.getWeekyear, currentMonday.getWeekOfWeekyear)).toList
    result
  }

  def fromToWeek(year: Int, week: Int): (String, String) = {
    val firstDay = getFirstDayInWeek(year, week)
    val lastDay = firstDay plusDays 6
    val start = ("%02d." format firstDay.getDayOfMonth) +
      (if (firstDay.getMonthOfYear != lastDay.getMonthOfYear) ("%02d." format firstDay.getMonthOfYear)
      else "") +
      (if (firstDay.getYear != lastDay.getYear) ("%d" format firstDay.getYear)
      else "")
    val end = "%02d.%02d.%d" format (lastDay.getDayOfMonth, lastDay.getMonthOfYear, lastDay.getYear)
    (start, end)
  }

  def monthsInQuarter(year: Int, quarter: Int): List[Int] = {
    val start = (quarter - 1) * 3 + 1
    (start to (start + 2)).toList
  }
}
