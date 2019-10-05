package de.gellien.timeplanner.timeplan
import java.time.LocalDate

import org.specs2.mutable._
class TimeSpec extends SpecificationWithJUnit {

  "weeks in year" should {
    "be 52 for 2001" in { TimeHelper.weeksInYear(2001) must_== 52 }
    "be 52 for 2002" in { TimeHelper.weeksInYear(2002) must_== 52 }
    "be 52 for 2003" in { TimeHelper.weeksInYear(2003) must_== 52 }
    "be 53 for 2004" in { TimeHelper.weeksInYear(2004) must_== 53 }
    "be 52 for 2005" in { TimeHelper.weeksInYear(2005) must_== 52 }
    "be 52 for 2006" in { TimeHelper.weeksInYear(2006) must_== 52 }
    "be 52 for 2007" in { TimeHelper.weeksInYear(2007) must_== 52 }
    "be 52 for 2008" in { TimeHelper.weeksInYear(2008) must_== 52 }
    "be 53 for 2009" in { TimeHelper.weeksInYear(2009) must_== 53 }
    "be 52 for 2010" in { TimeHelper.weeksInYear(2010) must_== 52 }
    "be 52 for 2011" in { TimeHelper.weeksInYear(2011) must_== 52 }
    "be 52 for 2012" in { TimeHelper.weeksInYear(2012) must_== 52 }
    "be 52 for 2013" in { TimeHelper.weeksInYear(2013) must_== 52 }
    "be 52 for 2014" in { TimeHelper.weeksInYear(2014) must_== 52 }
    "be 53 for 2015" in { TimeHelper.weeksInYear(2015) must_== 53 }
    "be 52 for 2016" in { TimeHelper.weeksInYear(2016) must_== 52 }
  }
  
  "month in quarter" should {
    "return 1 to 3 for Q1" in {TimeHelper.monthsInQuarter(2012, 1) must_== (1 to 3).toList}
    "return 4 to 6 for Q2" in {TimeHelper.monthsInQuarter(2012, 2) must_== (4 to 6).toList}
    "return 7 to 9 for Q3" in {TimeHelper.monthsInQuarter(2012, 3) must_== (7 to 9).toList}
    "return 10 to 12 for Q4" in {TimeHelper.monthsInQuarter(2012, 4) must_== (10 to 12).toList}
  }

  "getWeekyear" should {
    /*
    for (year <- 2000 to 2020) {
      val dayOfWeek = LocalDate.of(year, 12, 31).getDayOfWeek
      println(f"$year-12-31 is a $dayOfWeek")
    }
2000-12-31 is a SUNDAY
2001-12-31 is a MONDAY
2002-12-31 is a TUESDAY
2003-12-31 is a WEDNESDAY
2004-12-31 is a FRIDAY
2005-12-31 is a SATURDAY
2006-12-31 is a SUNDAY
2007-12-31 is a MONDAY
2008-12-31 is a WEDNESDAY
2009-12-31 is a THURSDAY
2010-12-31 is a FRIDAY
2011-12-31 is a SATURDAY
2012-12-31 is a MONDAY
2013-12-31 is a TUESDAY
2014-12-31 is a WEDNESDAY
2015-12-31 is a THURSDAY
2016-12-31 is a SATURDAY
2017-12-31 is a SUNDAY
2018-12-31 is a MONDAY
2019-12-31 is a TUESDAY
2020-12-31 is a THURSDAY
     */
    "give next year for last day is a Tuesday" in {TimeHelper.getWeekyear(LocalDate.of(2019, 12, 31)) must_== 2020}
    "give next year for last day is a Wednesday" in {TimeHelper.getWeekyear(LocalDate.of(2014, 12, 31)) must_== 2015}
    "give same year for last day is a Thursday" in {TimeHelper.getWeekyear(LocalDate.of(2020, 12, 31)) must_== 2020}
    "give same year when first day is a Wednesday" in {TimeHelper.getWeekyear(LocalDate.of(2020, 1, 1)) must_== 2020}
    "give same year when first day is a Thursday" in {TimeHelper.getWeekyear(LocalDate.of(2015, 1, 1)) must_== 2015}
    "give last year when first day is a Friday" in {TimeHelper.getWeekyear(LocalDate.of(2016, 1, 1)) must_== 2015}
  }

}