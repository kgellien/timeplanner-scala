package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan.PeriodPlan
import de.gellien.timeplanner.timeplan.WeekPlan

class LatexWeekSchedules {
  def render(plans: List[PeriodPlan], conf: WeekPlanConf, weekSchedule: WeekSchedule) = {
    val result = new ListBuffer[String]
    result ++= LatexHeader.header(conf)
    result += "\\begin{document}"
    for (plan <- plans) {
      plan match {
        case weekPlan: WeekPlan => result ++= weekSchedule.render(weekPlan, conf)
        case _                  => //println(s"Ignore ${plan.period}")
      }
    }
    result += """\end{document}"""
    result
  }
}

object LatexWeekScheduleRenderer {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf, offset: Int, amount: Int, cellHeight: String) = {
    val result = new ListBuffer[String]
    val (left, middle, rightCandidate) = weekPlan.header
    val right = if (rightCandidate == "") """\phantom{.}""" else rightCandidate
    result += """{\Large \bf %s \hfill %s \hfill %s}\\""" format (left, middle.replace(" - ", " -- "), right)
    result += s"""\\begin{tabular}{|r|*{7}{p{${conf.daywidthWs}}|}}"""
    result += """\hline"""
    result += """Zeit & Montag & Dienstag & Mittwoch & Donnerstag & Freitag & Samstag & Sonntag\\ \hline \hline"""
    for (i <- offset to (offset + amount)) {
      result += s""" ${i} -- ${i + 1} & & & & & & & \\\\[${cellHeight}] \\hline"""
    }
    result += """\end{tabular}"""
    result += """\newpage"""
    result
  }
}

sealed trait WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String]
}

object LatexWeekWorkPlan extends WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String] = {
    render(weekPlan, conf, "1.10cm")
  }
  def render(weekPlan: WeekPlan, conf: WeekPlanConf, cellHeight: String): ListBuffer[String] = {
    val result = new ListBuffer[String]
    val (left, middle, rightCandidate) = weekPlan.header
    val right = if (rightCandidate == "") """\phantom{.}""" else rightCandidate
    result += """{\Large \bf %s \hfill %s \hfill %s}\\""" format (left, middle.replace(" - ", " -- "), right)
    result += s"""\\begin{tabular}{|p{4cm}|*{7}{p{${conf.daywidthWw}}|}}"""
    result += """\hline"""
    result += """Aufgabenbeschreibung & Montag & Dienstag & Mittwoch & Donnerstag & Freitag & Samstag & Sonntag\\ \hline \hline"""
    for (i <- 1 to 10) {
      result += s""" & & & & & & & \\\\[${cellHeight}] \\hline"""
    }
    result += """\end{tabular}"""
    result += """\newpage"""
    result
  }
}

object LatexWeekSchedule extends WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String] = {
    LatexWeekScheduleRenderer.render(weekPlan, conf, 7, 13, "0.65cm")
  }
}

object LatexWeekSchedule24 extends WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String] = {
    LatexWeekScheduleRenderer.render(weekPlan, conf, 0, 11, "0.80cm") ++ LatexWeekScheduleRenderer.render(weekPlan, conf, 12, 11, "0.80cm")
  }
}
