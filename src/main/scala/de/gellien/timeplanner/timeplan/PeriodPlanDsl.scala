package de.gellien.timeplanner.timeplan

import scala.util.parsing.combinator.JavaTokenParsers

class PeriodPlanDsl(workList: List[String], val withOverviewDefault: Boolean) extends JavaTokenParsers {

  lazy val periodplan = (validPeriodplan | ignore)

  lazy val validPeriodplan = (weekLineWithOverview | weekLine | monthLineWithOverview | monthLine | quarterLineWithOverview | quarterLine | yearLineWithOverview | yearLine)

  // TODO: find a better way to make withOverview optional!
  lazy val weekLineWithOverview = ("Week" | "W") ~> year ~ weekNo ~ withOverview ^^ {case year ~ weekNo ~ withOverview => Some(WeekPlan(year, weekNo, workList, withOverview))}
  
  lazy val weekLine = ("Week" | "W") ~> year ~ weekNo ^^ {case year ~ weekNo => Some(WeekPlan(year, weekNo, workList, withOverviewDefault))}
  
  lazy val monthLineWithOverview = ("Month" | "M") ~> year ~ monthNo ~ withOverview ^^ {case year ~ monthNo ~ withOverview => Some(MonthPlan(year, monthNo, workList, withOverview))}
  
  lazy val monthLine = ("Month" | "M") ~> year ~ monthNo ^^ {case year ~ monthNo => Some(MonthPlan(year, monthNo, workList, withOverviewDefault))}
  
  lazy val quarterLineWithOverview = ("Quarter" | "Q") ~> year ~ quarterNo ~ withOverview ^^ {case year ~ quarterNo ~ withOverview => Some(QuarterPlan(year, quarterNo, workList, withOverview))}
  
  lazy val quarterLine = ("Quarter" | "Q") ~> year ~ quarterNo ^^ {case year ~ quarterNo => Some(QuarterPlan(year, quarterNo, workList, withOverviewDefault))}
  
  lazy val yearLineWithOverview = ("Year" | "Y") ~> year ~ withOverview ^^ {case year ~ withOverview => Some(YearPlan(year, workList, withOverview))}
  
  lazy val yearLine = ("Year" | "Y") ~> year ^^ {case year => Some(YearPlan(year, workList, withOverviewDefault))}
  
  lazy val ignore = """.*""".r ^^ {case _ => None}
  
  
  lazy val year = """\d{4}""".r ^^ {case year => year.toInt}
    
  lazy val weekNo = """\d\d?""".r ^? ({case weekNo if ((1 to 53) contains weekNo.toInt) => weekNo.toInt}, (weekNo => "week needs to be in the range of 1 to 53"))

  lazy val monthNo = """\d\d?""".r ^? ({case monthNo if ((1 to 12) contains monthNo.toInt) => monthNo.toInt}, (monthNo => "month needs to be in the range of 1 to 12"))

  lazy val quarterNo = """\d\d?""".r ^? ({case quarterNo if ((1 to 4) contains quarterNo.toInt) => quarterNo.toInt}, (quarterNo => "quarter needs to be in the range of 1 to 4"))

  
  lazy val withOverview = trueFalse
  
  lazy val trueFalse = True | False

  lazy val True = ("true" | "t") ^^ {case _ => true}
  
  lazy val False = ("false" | "f") ^^ {case _ => false}
}

object PeriodPlanDsl {
  def getPeriodPlan(line: String, todoList: List[String], withOverview: Boolean): Either[String, Option[PeriodPlan]] = {
    val tpd = new PeriodPlanDsl(todoList, withOverview)
    val result = tpd.parseAll(tpd.periodplan, line) match {
      case tpd.Success(periodPlan, _) => Right(periodPlan)
      case tpd.Failure(msg, _) => Left("Failure: " + msg)
      case tpd.Error(msg, _) => Left("Error: " + msg)
    }
    result
  }
}
