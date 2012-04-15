package de.gellien.timeplanner.timeplan

import org.specs2.mutable.SpecificationWithJUnit

class ToDoListDslSpec extends SpecificationWithJUnit {

  val task01 = """W "weekly task""""
  val task01Expected = """(WeeklyEntry(),"weekly task")"""
  val task01a = """W weekly task"""
  val task01aExpected = """(WeeklyEntry(),"weekly task")"""
  val task02 = """2012-KW-04 "weekly task""""
  val task02Expected = """(WeekEntry(2012-KW-04),"weekly task")"""

  val daily = """D "daily task""""
  val dailyExpected = """(DailyEntry(),"daily task")"""
  val mondays = """1 "mondays""""
  val mondaysExpected = """(WeekDayEntry(1),"mondays")"""
  val specificDate = """2012-01-26 "specific day in january""""
  val specificDateExpected = """(DayEntry(2012-01-26),"specific day in january")"""
  val dailyWithTime = """D 11:00 "daily task""""
  val dailyWithTimeExpected = """(DailyEntry(),"11:00 daily task")"""
  val dailyWithTimePeriod = """D 11:00 -- 12:00 "daily task""""
  val dailyWithTimePeriodExpected = """(DailyEntry(),"11:00 -- 12:00 daily task")"""
  val dailyWithTimePeriod2 = """D 11:00 - 12:00 "daily task""""
  val dailyWithTimePeriod2Expected = """(DailyEntry(),"11:00 - 12:00 daily task")"""
    
  def getParseResultAsString(task: String, expected: String, debug: Boolean) = {
      if (debug) println("#  expected: " + expected)
      val tpd = new ToDoListDsl
      val result = tpd.parseAll(tpd.toDoItem, task) match {
        case tpd.Success(toDoItem, _) => toDoItem.toString
        case tpd.Failure(msg, _) => "Failure: " + msg
        case tpd.Error(msg, _) => "Error: " + msg
      }
      if (debug) println("## result:   " + result)
      result
  }

  def check_==(task: String, expected: String, debug: Boolean = false) =
      getParseResultAsString(task, expected, debug) must_== expected

  def check_!=(task: String, expected: String, debug: Boolean = false) =
      getParseResultAsString(task, expected, debug) must_!= expected

  "ToDoListDsl" should {
    "parse weekly entry" in {
      check_==(task01, task01Expected)
    }

    "parse calendar week" in {
      check_==(task02, task02Expected)
    }

    "task without enclosing double-ticks leads to Failure" in {
      check_!=(task01a, task01aExpected)
    }

    "parse daily" in {
      check_==(daily, dailyExpected)
    }

    "parse mondays" in {
      check_==(mondays, mondaysExpected)
    }
    
    "parse specific date" in {
      check_==(specificDate, specificDateExpected)
    }

    "parse daily with time" in {
      check_==(dailyWithTime, dailyWithTimeExpected)
    }

    "parse daily with time period (LaTeX-Style)" in {
      check_==(dailyWithTimePeriod, dailyWithTimePeriodExpected)
    }

    "parse daily with time period" in {
      check_==(dailyWithTimePeriod2, dailyWithTimePeriod2Expected)
    }

/*    
*/
  }
}