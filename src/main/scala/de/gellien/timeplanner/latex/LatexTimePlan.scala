package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan._
import scala.collection.mutable.Map

object LatexTimePlan {
  val totalWidth = List(280, 275, 267, 262, 258, 255, 248.15)
  def getColumnWidth(numberOfColumns: Int) =
    "%4.2fmm" format (totalWidth(numberOfColumns - 1) / (numberOfColumns))
}

class LatexTimePlan(plans: List[PeriodPlan], withSeparator: Boolean) {
  val preamble = """\oddsidemargin 1cm
\evensidemargin 1cm
\topmargin 0cm
\addtolength{\oddsidemargin}{-2.54cm}
\addtolength{\evensidemargin}{-2.54cm}
\addtolength{\topmargin}{-2.54cm}
\textwidth 27.7cm
\textheight18cm
\pagestyle{empty}
\parindent 0pt
"""

  def render = {
    val result = new ListBuffer[String]
    result += """\documentclass[12pt,a4paper,landscape]{article}"""
    result += "\\usepackage[landscape]{geometry}" // escape does not work in """ """, neither does single backslash; backslash-u as Unicode-escape?!?
    result += "\\usepackage[latin1]{inputenc}"
    result += "\\usepackage[german]{babel}"
    result += "\\usepackage[T1]{fontenc}"
    result += "\\usepackage{times}"
    result += "\\usepackage{multicol}"
    result += "\\usepackage{rotating}"
    result += preamble
    result += """\begin{document}"""
    for (plan <- plans) result.appendAll(renderSinglePlan(plan))
    result += """\end{document}"""
    result
  }

  def renderSinglePlan(plan: PeriodPlan) = {
    val result = new ListBuffer[String]
    result.appendAll(renderHeader(plan))
    if (plan.withOverview) {
      result.appendAll(renderSinglePeriods("6cm", """\normalsize""", plan.periodOverview))
      result.appendAll(renderSinglePeriods("10cm", """\small""", plan.periodSpecifics))
    } else {
      result.appendAll(renderSinglePeriods("17cm", """\small""", plan.periodSpecifics))
    }
    result += "\\newpage"
    result
  }

  def renderHeader(plan: PeriodPlan) = {
    val (left, middle, right) = plan.header
    val result = new ListBuffer[String]
    result += """{\Large \bf %s \hfill %s \hfill %s}""" format (left, middle, right)
    result
  }

  def renderSinglePeriods(height: String, textsize: String, singlePeriods: List[SinglePeriod]) = {
    val result = new ListBuffer[String]
    val width = LatexTimePlan.getColumnWidth(singlePeriods.size)
    result += """\begin{multicols}{%d}""" format singlePeriods.size
    result.appendAll((for (singlePeriod <- singlePeriods) yield renderSinglePeriod(singlePeriod, height, width, textsize)).flatten)
    result += """\end{multicols}"""
    result
  }

  def renderSinglePeriod(singlePeriod: SinglePeriod, height: String, width: String, textsize: String) = {
    val result = new ListBuffer[String]
    result += """\framebox{"""
    result += """\begin{minipage}[t][%s]{%s}%s""" format (height, width, textsize)
    result.appendAll(renderHeading(singlePeriod))
    result.appendAll(renderTodoList(singlePeriod.todo))
    result += """\end{minipage}"""
    result += "}"
    result
  }

  def renderTodoList(todo: ToDoList) = {
    val result = new ListBuffer[String]
    if (!todo.anniversaries.isEmpty) {
      result += todo.anniversaries.mkString("\n\n")
      result += """{\center \rule{0.5\linewidth}{0.3mm}\\ } \vspace*{1em}"""
    }
    result += todo.appointments.mkString("\n\n")
    if (withSeparator) {
      result += """{\center \rule{0.5\linewidth}{0.3mm}\\ }"""
    }
    result += """\vfill"""
    val tasks = for (task <- todo.tasks) yield "- %s\n" format task
    result.appendAll(tasks)
    result
  }

  def renderHeading(singlePeriod: SinglePeriod) = {
    val result = new ListBuffer[String]
    result += """\begin{center}"""
    result += """{\bf %s} \\""" format singlePeriod.header
    result += """\rule{\linewidth}{0.3mm} \\"""
    result += """\end{center}"""
    result
  }
}
