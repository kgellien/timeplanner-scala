package de.gellien.timeplanner.timeplan

import org.specs2.mutable._

class ToDoSpec extends SpecificationWithJUnit {
  val eps = 0.00001
  "timediff" should {
    "work for diff of 45 Minutes" in {
      DateTimeHelper.timeDiff(Time(10, 5), Time(10, 50)) must beCloseTo(0.75, eps)
    }
    "work for diff of 1 hour" in {
      DateTimeHelper.timeDiff(Time(10, 0), Time(11, 0)) must beCloseTo(1, eps)
    }
  }

}
