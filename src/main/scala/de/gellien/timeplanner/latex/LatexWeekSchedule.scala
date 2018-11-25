package de.gellien.timeplanner.latex

import de.gellien.timeplanner.timeplan.WeekPlan
import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan.PeriodPlan

class LatexWeekSchedules {
  val preamble = """
\addtolength{\oddsidemargin}{-1in}
\addtolength{\evensidemargin}{-1in}
\addtolength{\topmargin}{-1in}
\pagestyle{empty}
\parindent 0pt
"""
  def render(plans: List[PeriodPlan], conf: WeekPlanConf, weekSchedule: WeekSchedule) = {
    val result = new ListBuffer[String]
    result += "\\documentclass[12pt,a4paper,landscape]{article}"
    result += "\\usepackage[landscape]{geometry}"
    result += s"\\usepackage[${conf.encoding}]{inputenc}"
    result += "\\usepackage[ngerman]{babel}"
    result += "\\usepackage[T1]{fontenc}"
    result += "\\usepackage{times}"
    result += s"\\setlength{\\textheight}{${conf.pageHeight}}"
    result += s"\\setlength{\\textwidth}{${conf.pageWidth}}"
    result += s"\\oddsidemargin  ${conf.oddsidemargin}"
    result += s"\\evensidemargin ${conf.evensidemargin}"
    result += s"\\topmargin      ${conf.topmargin}"
    result += preamble
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

sealed trait WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String]
}

object LatexWeekSchedule extends WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String] = {
    render(weekPlan, conf, 7)
  }
  def render(weekPlan: WeekPlan, conf: WeekPlanConf, offset: Int) = {
    val result = new ListBuffer[String]
    val (left, middle, rightCandidate) = weekPlan.header
    val right = if (rightCandidate == "") """\phantom{.}""" else rightCandidate
    result += """{\Large \bf %s \hfill %s \hfill %s}\\""" format (left, middle.replace(" - ", " -- "), right)
    result += s"""\\begin{tabular}{|r|*{7}{p{${conf.daywidthWs}}|}}"""
    result += """\hline"""
    result += """Zeit & Montag & Dienstag & Mittwoch & Donnerstag & Freitag & Samstag & Sonntag\\ \hline \hline"""
    for (i <- offset to (offset + 13)) {
      result += s""" ${i} -- ${i + 1} & & & & & & & \\\\[0.65cm] \\hline"""
    }
    result += """\end{tabular}"""
    result += """\newpage"""
    result
  }
}

object LatexWeekSchedule24 extends WeekSchedule {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf): ListBuffer[String] = {
    render(weekPlan, conf, 0) ++ render(weekPlan, conf, 12)
  }
  def render(weekPlan: WeekPlan, conf: WeekPlanConf, offset: Int) = {
    val result = new ListBuffer[String]
    val (left, middle, rightCandidate) = weekPlan.header
    val right = if (rightCandidate == "") """\phantom{.}""" else rightCandidate
    result += """{\Large \bf %s \hfill %s \hfill %s}\\""" format (left, middle.replace(" - ", " -- "), right)
    result += s"""\\begin{tabular}{|r|*{7}{p{${conf.daywidthWs}}|}}"""
    result += """\hline"""
    result += """Zeit & Montag & Dienstag & Mittwoch & Donnerstag & Freitag & Samstag & Sonntag\\ \hline \hline"""
    for (i <- offset to (offset + 11)) {
      result += s""" ${i} -- ${i + 1} & & & & & & & \\\\[0.80cm] \\hline"""
    }
    result += """\end{tabular}"""
    result += """\newpage"""
    result
  }
}
