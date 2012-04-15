package de.gellien.timeplanner.timeplan
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

}