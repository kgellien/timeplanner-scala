package de.gellien.timeplanner.timeplan

import org.specs2.mutable.SpecificationWithJUnit

class TimePlanDslSpec extends SpecificationWithJUnit {
  val week01 = """2012-W05"""
  val week01Expected = """Some(WeekTimePlan(2012, 5))"""
  val month01 = """2013-01"""
  val month01Expected = """Some(MonthTimePlan(2013, 1))"""
  val quarter01 = """2013-Q1"""
  val quarter01Expected = """Some(QuarterTimePlan(2013, 1))"""
  val quarter02 = """2013-Q01"""
  val quarter02Expected = """Some(QuarterTimePlan(2013, 1))"""
  val year01 = """2013"""
  val year01Expected = """Some(YearTimePlan(2013))"""

  def getParseResultAsString(task: String, expected: String, debug: Boolean) = {
      if (debug) println("#  expected: " + expected)
      val tpd = new TimePlanDsl(7)
      val result = tpd.parseAll(tpd.timeplan, task) match {
        case tpd.Success(timePlanItem, _) => timePlanItem.toString
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

  "TimePlanDsl" should {
    "parse simple week entry" in {
      check_==(week01, week01Expected)
    }
    "parse simple month entry" in {
      check_==(month01, month01Expected)
    }
    "parse simple quarter entry" in {
      check_==(quarter01, quarter01Expected)
    }
    "parse quarter entry with leading 0" in {
      check_==(quarter02, quarter02Expected)
    }
    "parse simple year entry" in {
      check_==(year01, year01Expected)
    }
  }
}