package de.gellien.timeplanner.timeplan

import org.specs2.mutable._

class BoundCheckerSpec extends SpecificationWithJUnit {

  "withinBounds for Year" should {
    val year = 2015
    val pe = YearEntry(year)
    "be false for same year in GtBound" in {
      pe.withinBounds(List(GtBound(pe))) must_== false
    }
    "be true for previous year in GtBound" in {
      pe.withinBounds(List(GtBound(YearEntry(year-1)))) must_== true
    }
    "be true for same year in GeBound" in {
      pe.withinBounds(List(GeBound(pe))) must_== true
    }
    "be true for previous year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year-1)))) must_== true
    }
    "be false for same year in LtBound" in {
      pe.withinBounds(List(LtBound(pe))) must_== false
    }
    "be true for next year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year+1)))) must_== true
    }
    "be true for same year in LeBound" in {
      pe.withinBounds(List(LeBound(pe))) must_== true
    }
    "be true for next year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year+1)))) must_== true
    }
  }

  "withinBounds for Month" should {
    val year = 2015
    val month = 7
    val pe = MonthEntry(year, month)
    "be false for same year in GtBound" in {
      pe.withinBounds(List(GtBound(YearEntry(year)))) must_== false
    }
    "be true for previous year in GtBound" in {
      pe.withinBounds(List(GtBound(YearEntry(year-1)))) must_== true
    }
    "be true for same year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year)))) must_== true
    }
    "be true for previous year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year-1)))) must_== true
    }
    "be false for same year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year)))) must_== false
    }
    "be true for next year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year+1)))) must_== true
    }
    "be true for same year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year)))) must_== true
    }
    "be true for next year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year+1)))) must_== true
    }
  }

  "withinBounds for Day" should {
    val year = 2015
    val month = 7
    val day = 2
    val pe = DayEntry(year, month, day)
    "be false for same year in GtBound" in {
      pe.withinBounds(List(GtBound(YearEntry(year)))) must_== false
    }
    "be true for previous year in GtBound" in {
      pe.withinBounds(List(GtBound(YearEntry(year-1)))) must_== true
    }
    "be true for same year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year)))) must_== true
    }
    "be true for previous year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year-1)))) must_== true
    }
    "be false for same year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year)))) must_== false
    }
    "be true for next year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year+1)))) must_== true
    }
    "be true for same year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year)))) must_== true
    }
    "be true for next year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year+1)))) must_== true
    }
  }

}
