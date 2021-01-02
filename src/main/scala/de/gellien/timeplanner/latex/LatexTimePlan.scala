package de.gellien.timeplanner.latex

import de.gellien.timeplanner.config.Encodings

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan._

import java.util.Locale

object LatexTimePlan {
  // need to guarantee decimal point in output
  val locale = Locale.ENGLISH
  val totalWidth = List(275, 272, 267, 262, 258, 255, 248.15)
  def getColumnWidth(numberOfColumns: Int) = {
    val width = (1.0*totalWidth(numberOfColumns - 1)) / (numberOfColumns)
    "%4.2fmm" formatLocal (locale, width)
  }
}

class LatexTimePlan(plans: List[PeriodPlan], withSeparator: Boolean)(implicit encodings: Encodings) {
  def render = {
    val result = new ListBuffer[String]
    result ++= LatexHeader.header(TimePlanConf)
    result += """\begin{document}"""
    for (plan <- plans)
      result ++= renderSinglePlan(plan)
    result += """\end{document}"""
  }

  def renderSinglePlan(plan: PeriodPlan) = {
    val result = new ListBuffer[String]
    result ++= renderHeader(plan)
    if (plan.withOverview) {
      result ++= renderSinglePeriods("6cm", """\normalsize""", plan.periodOverview)
      result ++= renderSinglePeriods("10cm", """\small""", plan.periodSpecifics)
    } else {
      result ++= renderSinglePeriods("17cm", """\small""", plan.periodSpecifics)
    }
    result += "\\newpage"
  }

  def renderHeader(plan: PeriodPlan) = {
    val (left, middle, rightCandidate) = plan.header
    val right = if (rightCandidate == "") """\phantom{.}""" else rightCandidate
    val result = new ListBuffer[String]
    result += """{\Large \bf %s \hfill %s \hfill %s}""" format (left, middle.replace(" - ", " -- "), right)
  }

  def renderSinglePeriods(height: String, textsize: String, singlePeriods: List[SinglePeriod]) = {
    val result = new ListBuffer[String]
    val width = LatexTimePlan.getColumnWidth(singlePeriods.size)
    if (singlePeriods.size > 1)
      result += """\begin{multicols}{%d}""" format singlePeriods.size
    result ++= singlePeriods.flatMap(singlePeriod => renderSinglePeriod(singlePeriod, height, width, textsize))
    if (singlePeriods.size > 1)
      result += """\end{multicols}"""
    result
  }

  def renderSinglePeriod(singlePeriod: SinglePeriod, height: String, width: String, textsize: String) = {
    val result = new ListBuffer[String]
    result += """\framebox{"""
    result += """\begin{minipage}[t][%s]{%s}%s""" format (height, width, textsize)
    result ++= renderHeading(singlePeriod)
    result ++= renderTodoList(singlePeriod.todo)
    result += """\end{minipage}"""
    result += "}"
  }

  def renderTodoList(todo: ToDoList) = {
    def escapeSpecialChars(txt: String) = {
      txt.replace("&", "\\&").replace("%", "\\%").replace("_", "$_$")
    }
    val centeredLine = """{\center \rule{0.5\linewidth}{0.3mm}\\[1.5em] }"""
    val result = new ListBuffer[String]
    result += todo.appointments.sortBy(_.timeInfoSort).map { a =>
      if (a.timeInfo.from == a.timeInfo.to) {
        s"${a.timeInfo.from.short} ${a.info}"
      } else {
        s"${a.timeInfo.from.short} -- ${a.timeInfo.to.short} ${escapeSpecialChars(a.info)}"
      }
    }.mkString("\n\n")
    if (!todo.anniversaries.isEmpty) {
      result += """\vfill"""
      result += centeredLine
      result += todo.anniversaries.sortBy(_.info).map { a =>
        s"$$*$$${a.info}" + (a.yearOpt match {
          case Some(year) => f" (${year}%d)"
          case _          => ""
        })
      }.mkString("\n", "\n\n", "\n")
    }
    if (withSeparator || !todo.anniversaries.isEmpty) {
      result += centeredLine
    }
    result += """\vfill"""
    result += todo.tasks.sortBy(_.info).map { a =>
      val txt = escapeSpecialChars(a.info)
      s"""$$\\square$$ ${txt}"""
    }.mkString("\n\n")
    result
  }

  def renderHeading(singlePeriod: SinglePeriod) = {
    val result = new ListBuffer[String]
    result += """\begin{center}"""
    result += """{\bf %s} \\""" format singlePeriod.header.replace(" - ", " -- ")
    val specials = singlePeriod.todo.specials
    //    result ++= specials map ("""%s \\""" format _.info)
    for (special <- specials) {
      result += """%s \\""" format special.info
    }
    if (specials.size == 0)
      result += "\\"
    result += """\rule{\linewidth}{0.3mm} \\"""
    result += """\end{center}"""
  }
}
