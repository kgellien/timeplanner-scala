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
    result append "\\newpage"
    result
  }

  def renderHeader(plan: PeriodPlan) = {
    val (left, middle, right) = plan match {
      case WeekPlan(year, week, workList, full) =>
        val (start, end) = TimeHelper.fromToWeek(year, week)
        val middle = "%s -- %s" format (start, end)
        ("%d" format year, middle, "W%02d" format week)
      case MonthPlan(year, month, workList, full) => ("%d" format year, "", "%s" format (TimeHelper.monthName(month)))
      case QuarterPlan(year, quarter, workList, full) => ("%d" format year, "", "%s" format "Q%d" format quarter)
      case YearPlan(year, workList, full) => ("%d" format year, "", "")
      case _ => ("left", "middle", "right")
    }
    val result = new ListBuffer[String]
    result.append("""{\Large \bf %s \hfill %s \hfill %s}""" format (left, middle, right))
    result
  }

  def renderSinglePeriods(height: String, textsize: String, singlePeriods: List[SinglePeriod]) = {
    val result = new ListBuffer[String]
    val width = LatexTimePlan.getColumnWidth(singlePeriods.size)
    result.append("""\begin{multicols}{%d}""" format singlePeriods.size)
    result.appendAll((for (singlePeriod <- singlePeriods) yield renderSinglePeriod(singlePeriod, height, width, textsize)).flatten)
    result.append("""\end{multicols}""")
    result
  }

  def renderSinglePeriod(singlePeriod: SinglePeriod, height: String, width: String, textsize: String) = {
    val result = new ListBuffer[String]
    result.append("""\framebox{""")
    result.append("""\begin{minipage}[t][%s]{%s}%s""" format (height, width, textsize))
    result.appendAll(renderHeading(singlePeriod))
    result.appendAll(renderWorklist(singlePeriod.todo))
    result.append("""\end{minipage}""")
    result.append("}")
    result
  }

  def renderWorklist(todo: ToDoList) = {
    val result = new ListBuffer[String]
    if (!todo.anniversaries.isEmpty) {
      result.append(todo.anniversaries.mkString("\n\n"))
      result.append("""{\center \rule{0.5\linewidth}{0.3mm}\\ } \vspace*{1em}""")
    }
    result.append(todo.appointments.mkString("\n\n"))
    if (withSeparator) {
      result.append("""{\center \rule{0.5\linewidth}{0.3mm}\\ }""")
    }
    result.append("""\vfill""")
    result.append(todo.tasks.mkString("\n\n"))
    result
  }

  def renderHeading(singlePeriod: SinglePeriod) = {
    val header = singlePeriod.header
    val result = new ListBuffer[String]
    result.append("""\begin{center}""")
    val headerLines = header match {
      case Some(line) => List(line)
      case None       => getDefaultHeading(singlePeriod)
    }
    for (line <- headerLines) result.append("""{\bf %s} \\""" format line)
    result.append("""\rule{\linewidth}{0.3mm} \\""")
    result.append("""\end{center}""")
    result
  }

  def getDefaultHeading(singlePeriod: SinglePeriod) = {
    singlePeriod match {
      case Day(year, month, day, _, _) => List(TimeHelper.displayDay(year, month, day))
      case Week(year, week, _, _) =>
        val (from, to) = TimeHelper.fromToWeek(year, week)
        val fromTo = "%s -- %s" format (from, to)
        List("W%02d" format week, fromTo)
      case Month(year, month, _, _)     => List("%s" format (TimeHelper.monthName(month)))
      case Quarter(year, quarter, _, _) => List("Q%d" format quarter)
      case Year(year, _, _)             => List("%d" format year)
      case _                            => List("MyHeader") // fallback; should not happen
    }
  }
}
