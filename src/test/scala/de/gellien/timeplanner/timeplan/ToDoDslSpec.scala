package de.gellien.timeplanner.timeplan

import org.specs2.mutable.SpecificationWithJUnit

class ToDoDslSpec extends SpecificationWithJUnit {

  val task01 = """W weekly task"""
  val task01Expected = """Task(W,None,List(),weekly task)"""
  val task01a = """W weekly task"""
  val task01aExpected = """Task(W,None,List(),weekly task)"""
  val task02 = """2012-W04 weekly task"""
  val task02Expected = """Task(2012-W04,None,List(),weekly task)"""

  val daily = """D daily task"""
  val dailyExpected = """Task(D,None,List(),daily task)"""
  val mondays = """D1 mondays"""
  val mondaysExpected = """Task(D1,None,List(),mondays)"""
  val mondaysWithSeparatedBound = """D1 = 2012-04 mondays in april"""
  val mondaysWithSeparatedBoundExpected = """Task(D1,None,List(EqBound(2012-04)),mondays in april)"""
  val mondaysWithBounds = """D1 =2012-04 !2014-04-23 mondays in april"""
  val mondaysWithBoundsExpected = """Task(D1,None,List(EqBound(2012-04), NeBound(2014-04-23)),mondays in april)"""
  val specificDate = """2012-01-26 specific day in january"""
  val specificDateExpected = """Task(2012-01-26,None,List(),specific day in january)"""
  val specificDateWithClassifier = """2012-01-26 [aikido] classified task"""
  val specificDateExpectedWithClassifier = """Task(2012-01-26,Some(aikido),List(),classified task)"""
  val dailyWithTime = """D 11:00 daily task"""
  val dailyWithTimeExpected = """Appointment(D,None,11:00,daily task)"""
  val dailyWithTimePeriod = """D 11:00 -- 12:00 daily task"""
  val dailyWithTimePeriodExpected = """Appointment(D,None,11:00 -- 12:00,daily task)"""
  val dailyWithTimePeriod2 = """D 11:00 - 12:00 daily task"""
  val dailyWithTimePeriod2Expected = """Appointment(D,None,11:00 - 12:00,daily task)"""

  def getParseResultAsString(task: String, expected: String, debug: Boolean) = {
    if (debug) println("#  expected: " + expected)
    val tpd = new ToDoDsl
    val result = tpd.parseAll(tpd.toDoItem, task) match {
      case tpd.Success(toDoItem, _) => toDoItem.toString
      case tpd.Failure(msg, _)      => "Failure: " + msg
      case tpd.Error(msg, _)        => "Error: " + msg
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

    //    "task without enclosing double-ticks leads to Failure" in {
    //      check_!=(task01a, task01aExpected)
    //    }

    "parse daily" in {
      check_==(daily, dailyExpected)
    }

    "parse mondays" in {
      check_==(mondays, mondaysExpected)
    }

    "parse mondays with EqBound and NeBound" in {
      check_==(mondaysWithBounds, mondaysWithBoundsExpected)
    }

    "parse mondays with separated EqBound" in {
      check_==(mondaysWithSeparatedBound, mondaysWithSeparatedBoundExpected)
    }

    "parse specific date" in {
      check_==(specificDate, specificDateExpected)
    }

    "parse specific date with classifier" in {
      check_==(specificDateWithClassifier, specificDateExpectedWithClassifier)
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
  }
}
