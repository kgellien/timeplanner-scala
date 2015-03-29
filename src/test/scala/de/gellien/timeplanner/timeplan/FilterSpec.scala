package de.gellien.timeplanner.timeplan

import org.specs2.mutable.SpecificationWithJUnit

class FilterSpec extends SpecificationWithJUnit {

  def createOverview = {
    val workList = List("2012-KW-03 P private test")
    val wtp = WeekTimePlan(2012, 3)(7)
    val wp = wtp.createPeriodPlan(workList, true)
    wp.periodOverview
  }
  
  "PeriodPlan.periodOverview" should {
    "contain exactly three entries" in {
      val overview = createOverview
      overview.size must_== 3
    }

    "contain exactly three entries" in {
      val overview = createOverview
      overview.size must_== 3
    }
  }

  "PeriodPlan.splitPeriod" should {
    "split into exactly three periods" in {
      val workList = ToDoList(List(), List("2012-KW-03 P private test"), List())
      val week = Week(2012, 3, workList)
      val overview = PeriodSplitter.splitPeriod(week)
      overview.size must_== 3
    }

  }

}