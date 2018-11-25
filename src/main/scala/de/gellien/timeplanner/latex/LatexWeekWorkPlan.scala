package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan.PeriodPlan
import de.gellien.timeplanner.timeplan.WeekPlan

class LatexWeekWorkPlans {
  val preamble = """
\addtolength{\oddsidemargin}{-1in}
\addtolength{\evensidemargin}{-1in}
\addtolength{\topmargin}{-1in}
\pagestyle{empty}
\parindent 0pt
"""
  def render(plans: List[PeriodPlan], conf: WeekPlanConf) = {
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
        case weekPlan: WeekPlan => result ++= LatexWeekWorkPlan.render(weekPlan, conf)
        case _                  => //println(s"Ignore ${plan.period}")
      }
    }
    result += """\end{document}"""
    result
  }
}

object LatexWeekWorkPlan {
  def render(weekPlan: WeekPlan, conf: WeekPlanConf) = {
    val result = new ListBuffer[String]
    val (left, middle, rightCandidate) = weekPlan.header
    val right = if (rightCandidate == "") """\phantom{.}""" else rightCandidate
    result += """{\Large \bf %s \hfill %s \hfill %s}\\""" format (left, middle.replace(" - ", " -- "), right)
    result += s"""\\begin{tabular}{|p{4cm}|*{7}{p{${conf.daywidthWw}}|}}"""
    result += """\hline"""
    result += """Aufgabenbeschreibung & Montag & Dienstag & Mittwoch & Donnerstag & Freitag & Samstag & Sonntag\\ \hline \hline"""
    for (i <- 1 to 10) {
      result += """ & & & & & & & \\[1.10cm] \hline"""
    }
    result += """\end{tabular}"""
    result += """\newpage"""
    result
  }
}
