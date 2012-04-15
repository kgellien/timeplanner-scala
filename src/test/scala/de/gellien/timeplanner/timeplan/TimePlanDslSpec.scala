package de.gellien.timeplanner.timeplan

import org.specs2.mutable.SpecificationWithJUnit

class TimePlanDslSpec extends SpecificationWithJUnit {

  val week01 = """Week 2012 5"""
  val week01Expected = """Some(WeekPlan(2012,5,List(),true))"""

  def getParseResultAsString(task: String, expected: String, debug: Boolean) = {
      if (debug) println("#  expected: " + expected)
      val tpd = new PeriodPlanDsl(List())
      val result = tpd.parseAll(tpd.periodplan, task) match {
        case tpd.Success(timePlanItem, _) => timePlanItem.toString
        case tpd.Failure(msg, _) => "Failure: " + msg
        case tpd.Error(msg, _) => "Error: " + msg
      }
      if (debug) println("## result:   " + result)
      result
  }

  def check_==(task: String, expected: String, debug: Boolean = true) =
      getParseResultAsString(task, expected, debug) must_== expected

  def check_!=(task: String, expected: String, debug: Boolean = true) =
      getParseResultAsString(task, expected, debug) must_!= expected

  "TimePlanDsl" should {
    "parse simple week entry" in {
      check_==(week01, week01Expected)
    }

  }
}