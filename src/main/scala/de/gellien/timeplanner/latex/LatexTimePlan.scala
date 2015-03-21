package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan._
import scala.collection.mutable.Map

object LatexTimePlan {
  val totalWidth = List(280, 275, 267, 262, 258, 255, 248.15)
  def getColumnWidth(numberOfColumns: Int) =
    "%4.2fmm" format (totalWidth(numberOfColumns - 1) / (numberOfColumns))
}

class LatexTimePlan(tp: TimePlan, withSeparator: Boolean) {
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

  val plans = tp.periodPlans

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

  def renderSinglePeriods(height: String, textsize: String, periods: List[SinglePeriod]) = {
    val result = new ListBuffer[String]
    val width = LatexTimePlan.getColumnWidth(periods.size)
    result.append("""\begin{multicols}{%d}""" format periods.size)
    result.appendAll((for (singlePeriod <- periods) yield renderSinglePeriod(singlePeriod, height, width, textsize)).flatten)
    result.append("""\end{multicols}""")
    result
  }

  def renderSinglePeriod(item: SinglePeriod, height: String, width: String, textsize: String) = {
    val result = new ListBuffer[String]
    result.append("""\framebox{""")
    result.append("""\begin{minipage}[t][%s]{%s}%s""" format (height, width, textsize))
    result.appendAll(renderHeading(item))
    result.appendAll(renderWorklist(item))
    result.append("""\end{minipage}""")
    result.append("}")
    result
  }

  /* TODO: Refactor! SinglePeriod.todo should already be a map */
  def renderWorklist(item: SinglePeriod) = {
    val workLists: List[ToDoList] = item.todo
    val result = new ListBuffer[String]
    val toDoDict = Map[ToDoListType, List[String]]()
    for (workList <- workLists) {
      workList match {
        case ToDoList(Anniversary, todo) =>
          val prefix = "$*$"
          toDoDict(Anniversary) = todo.map(line => prefix + line + "\n")
        case ToDoList(Appointment, todo) =>
          val prefix = ""
          toDoDict(Appointment) = todo.map(line => prefix + line + "\n")
        case ToDoList(Task, todo) =>
          val prefix = "- "
          toDoDict(Task) = todo.map(line => prefix + line + "\n")
      }
    }

    val anniversaries = toDoDict getOrElse (Anniversary, List())
    val appointments = toDoDict getOrElse (Appointment, List())
    val tasks = toDoDict getOrElse (Task, List())

    // TODO: think through withSeparator
    if (!anniversaries.isEmpty) {
      result.appendAll(anniversaries)
      result.append("""{\center \rule{0.5\linewidth}{0.3mm}\\ } \vspace*{1em}""")
    }
    result.appendAll(appointments)
    if (withSeparator) {
      result.append("""{\center \rule{0.5\linewidth}{0.3mm}\\ }""")
    }
    result.append("""\vfill""")
    result.appendAll(tasks)

    result
  }

  def renderHeading(item: SinglePeriod) = {
    val header = item.header
    val result = new ListBuffer[String]
    result.append("""\begin{center}""")
    val headerLines = header match {
      case Some(line) => List(line)
      case None       => getDefaultHeading(item)
    }
    for (line <- headerLines) result.append("""{\bf %s} \\""" format line)
    result.append("""\rule{\linewidth}{0.3mm} \\""")
    result.append("""\end{center}""")
    result
  }

  def getDefaultHeading(item: SinglePeriod) = {
    item match {
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
