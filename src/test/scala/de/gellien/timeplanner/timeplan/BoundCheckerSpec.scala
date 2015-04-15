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
      pe.withinBounds(List(GtBound(YearEntry(year - 1)))) must_== true
    }
    "be true for same year in GeBound" in {
      pe.withinBounds(List(GeBound(pe))) must_== true
    }
    "be true for previous year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year - 1)))) must_== true
    }
    "be false for same year in LtBound" in {
      pe.withinBounds(List(LtBound(pe))) must_== false
    }
    "be true for next year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year + 1)))) must_== true
    }
    "be true for same year in LeBound" in {
      pe.withinBounds(List(LeBound(pe))) must_== true
    }
    "be true for next year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year + 1)))) must_== true
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
      pe.withinBounds(List(GtBound(YearEntry(year - 1)))) must_== true
    }
    "be true for same year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year)))) must_== true
    }
    "be true for previous year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year - 1)))) must_== true
    }
    "be false for same year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year)))) must_== false
    }
    "be true for next year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year + 1)))) must_== true
    }
    "be true for same year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year)))) must_== true
    }
    "be true for next year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year + 1)))) must_== true
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
      pe.withinBounds(List(GtBound(YearEntry(year - 1)))) must_== true
    }
    "be true for same year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year)))) must_== true
    }
    "be true for previous year in GeBound" in {
      pe.withinBounds(List(GeBound(YearEntry(year - 1)))) must_== true
    }
    "be false for same year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year)))) must_== false
    }
    "be true for next year in LtBound" in {
      pe.withinBounds(List(LtBound(YearEntry(year + 1)))) must_== true
    }
    "be true for same year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year)))) must_== true
    }
    "be true for next year in LeBound" in {
      pe.withinBounds(List(LeBound(YearEntry(year + 1)))) must_== true
    }
  }

  "withinBounds for multiple bounds" should {
    val eqb = EqBound(MonthEntry(2012, 4))
    val neb = NeBound(DayEntry(2012, 4, 16))
    val task = Task(WeekDayEntry(1), None, List(eqb, neb), "mondays in april")
    val excludedDay = DayEntry(2012, 4, 16)
    val includedDay = DayEntry(2012, 4, 23)
    "be false for excluded day (NeBound only)" in {
      excludedDay.withinBounds(List(neb)) must_== false
    }
    "be false for excluded day (combined bounds)" in {
      excludedDay.withinBounds(List(eqb, neb)) must_== false
    }
    "be true for included day (EqBound only)" in {
      includedDay.withinBounds(List(eqb)) must_== true
    }
    "be true for included day (NeBound only)" in {
      includedDay.withinBounds(List(neb)) must_== true
    }
    "be true for included day (combined bounds)" in {
      includedDay.withinBounds(List(eqb, neb)) must_== true
    }
  }

  "withinBounds for multiple bounds (lt and gt)" should {
    val gtb = GtBound(DayEntry(2015, 3, 28))
    val ltb = LtBound(DayEntry(2015, 4, 12))
    val excludedDay = DayEntry(2015, 4, 14)
    val includedDay = DayEntry(2015, 4, 7)
    "be true for excluded day (GtBound only)" in {
      excludedDay.withinBounds(List(gtb)) must_== true
    }
    "be false for excluded day (LtBound only)" in {
      excludedDay.withinBounds(List(ltb)) must_== false
    }
    "be false for excluded day (combined bounds)" in {
      excludedDay.withinBounds(List(gtb, ltb)) must_== false
    }
    "be true for included day (GtBound only)" in {
      includedDay.withinBounds(List(gtb)) must_== true
    }
    "be true for included day (LtBound only)" in {
      includedDay.withinBounds(List(ltb)) must_== true
    }
    "be true for included day (combined bounds)" in {
      includedDay.withinBounds(List(gtb, ltb)) must_== true
    }
  }

  "extractTodosForPeriod" should {
    val gtb = GtBound(DayEntry(2012, 4, 10))
    val ltb = LtBound(DayEntry(2012, 4, 20))
    val excludedDay = DayEntry(2012, 4, 24)
    val includedDay = DayEntry(2012, 4, 17)
    val task = Task(WeekDayEntry(2), None, List(gtb, ltb), "tuesdays in april")
    val appointment = Appointment(WeekDayEntry(2), None, List(gtb, ltb), "20:00", "tuesdays in april")
//    val todos: List[ToDo] = List(task)
    val todos: List[ToDo] = List(appointment)
    "return empty list for period outside bounds" in {
      val pe = excludedDay
      val pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(pe)), AnniversaryEntry(pe.month, pe.day))
      val psTodos = ToDoHelper.extractTodosForPeriod(pe, todos, pes: _*)
      println("out psTodos = %s" format psTodos)
//      psTodos.tasks.size must_== 0
      psTodos.appointments.size must_== 0
    }
    "return one element list for period inside bounds" in {
      val pe = includedDay
      val pes = List(DailyEntry(), WeekDayEntry(TimeHelper.getDayOfWeek(pe)), AnniversaryEntry(pe.month, pe.day))
      val psTodos = ToDoHelper.extractTodosForPeriod(pe, todos, pes: _*)
      println("in  psTodos = %s" format psTodos)
//      psTodos.tasks.size must_== 1
      psTodos.appointments.size must_== 1
    }
  }

}
