package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan._

class LatexDayPlans(plans: List[PeriodPlan], conf: DayPlanConf) {
  def render = {
    val result = new ListBuffer[String]
    result += """"""
    result += """\documentclass[a4paper,12pt]{article}"""
    result += "\\usepackage[latin1]{inputenc}"
    result += "\\usepackage{graphicx}"
    result += "\\setlength{\\headsep}{-3 cm}"
    result += "\\setlength{\\footskip}{1 cm}"
    result += s"\\setlength{\\textheight}{${conf.pageHeight} cm}"
    result += s"\\setlength{\\textwidth}{${conf.pageWidth} cm}"
    result += """\begin{document}"""
    result += """\pagestyle{empty}"""
    result += "\\setlength{\\unitlength}{10mm}"
    val latexSingleDayPlan = new LatexDayPlan(conf)
    for (plan <- plans) {
      plan match {
        case dayPlan: DayPlan =>
          result ++= latexSingleDayPlan.renderDayPlanPage(dayPlan)
        case weekPlan: WeekPlan =>
          println(s"daysPerWeek ${weekPlan.periodEntry.header}: ${weekPlan.daysPerWeek}")
          val dayPlans = for (singlePeriod <- weekPlan.periodSpecifics) yield {
            DayPlan(singlePeriod.periodEntry.asInstanceOf[DayEntry], singlePeriod, List[SinglePeriod](), false)
          }
          for (dayPlan <- dayPlans) {
            result ++= latexSingleDayPlan.renderDayPlanPage(dayPlan)
          }
        case plan => println(s"Ignore ${plan.period}")
      }
      //      result ++= latexSingleDayPlan.renderDayPlanPage(plan, Some(plan))
    }
    result += """\end{document}"""
    result
  }

}

class LatexDayPlan(conf: DayPlanConf) {

  def startI(i: Double) = conf.top - i * conf.hourLineDelta

  def createHourLines(hour: Int, i: Int) = {
    val result = new ListBuffer[String]
    val start = startI(i)
    val end = startI(i + 1)
    val startHalf = start - conf.hourLineDelta / 2.0
    val fullHour = start - 0.375 * conf.hourLineDelta
    val halfHour = start - 0.875 * conf.hourLineDelta
    result += s"% createHourLines(${hour}, ${i})"
    result += "% vertical hour lines"
    result += f"\\put(${conf.leftContent},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += f"\\put(${conf.middle},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += f"\\put(${conf.right},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += "% horizontal hour lines"
    result += f"\\put(${conf.leftContent},${start}){\\line(1,0){${conf.hourLength}}}"
    result += f"\\put(${conf.leftContent},${startHalf}){\\line(1,0){${conf.halfHourLength}}}"
    result
  }

  def createHourEntries(hour: Int, i: Int) = {
    val result = new ListBuffer[String]
    val start = startI(i)
    val end = startI(i + 1)
    val fullHour = start - 0.375 * conf.hourLineDelta
    val halfHour = start - 0.875 * conf.hourLineDelta
    result += s"% createHourEntries(${hour}, ${i})"
    result += "% vertical lines"
    result += f"\\put(${conf.left},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += f"\\put(${conf.leftContent},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += "% horizontal lines"
    result += f"\\put(${conf.left},${start}){\\line(1,0){${conf.leftContentWidth}}}"
    result += "% text"
    result += f"\\put(${conf.leftFullHour},${fullHour}){${conf.hourTextSize} ${hour}%02d:00}"
    result += f"\\put(${conf.leftHalfHour},${halfHour}){${conf.hourTextSize} :30}"
    result
  }

  def createHeader(header: String, withOffset: Boolean) = {
    val result = new ListBuffer[String]
    val posX = if (withOffset) conf.middle else conf.leftContent
    val posY = startI(-0.35)
    val width = conf.middle - conf.leftContent
    //    result += f"\\put(${posX},${posY}){\\scalebox{1.25}{\\makebox[${width} cm][c]{\\large \\textbf{${header}}}}}"
    result += f"\\put(${posX},${posY}){\\scalebox{1.25}{\\large \\textbf{${header}}}}"
    result
  }

  def escapeMsg(msg: String) = msg.replace("&", "\\&").replace("%", "\\%")

  def createTextEntry(msg: String, from: Time, to: Time, withOffset: Boolean) = {
    val result = new ListBuffer[String]
    val offsetY = conf.hourLineDelta * (from.mm / 60.0)
    val textX = (if (withOffset) conf.middle else conf.leftContent) + conf.leftFullHourWidth
    val i = from.hh - conf.firstHour
    val start = startI(i)
    val textY = start - offsetY - conf.textHeight
    result += f"\\put(${textX},${textY}){${conf.textSize} ${escapeMsg(msg)}}"
    // create left bracket
    val thicknessInCm = 0.15
    val posX = (if (withOffset) conf.middle else conf.leftContent) + thicknessInCm / 2.0
    val td = DateTimeHelper.timeDiff(from, to)
    val duration = if (Math.abs(td) < 0.01) conf.defaultDuration else td
    val length = conf.hourLineDelta * duration
    val posY = start - offsetY - length
    val tickThickness = thicknessInCm / 2.0
    val posYTick = posY + tickThickness / 2.0
    result += f"\\linethickness{${thicknessInCm} cm}"
    result += f"\\put(${posX},${posY}){\\line(0,1){${length}}}"
    result += f"\\linethickness{${tickThickness} cm}"
    result += f"\\put(${posX},${posYTick}){\\line(1,0){${thicknessInCm}}}"
    result += f"\\put(${posX},${posYTick + length - tickThickness}){\\line(1,0){${thicknessInCm}}}"
    result += f"\\linethickness{0.01cm}"
    result
  }

  def addTextEntries(msgs: List[Appointment], withOffset: Boolean) = {
    val startTime = Time(conf.firstHour, 0)
    val endTime = Time(conf.lastHour + 1, 0)
    def isRelevant(from: Time, to: Time) =
      (from.hh >= conf.firstHour && from.hh < conf.lastHour) ||
        (to.hh >= conf.firstHour && to.hh < conf.lastHour)
    def fromBar(from: Time) = if (from < startTime) startTime else from
    def toBar(to: Time) = if (to > endTime) endTime else to
    def msgTxt(msg: String, from: Time, to: Time) = {
      val start = if (from < startTime) from.toString else ""
      val end = if (to > endTime) to.toString else ""
      val suffix = if (start != "" || end != "") List(start, end).mkString(" (", " -- ", ")") else ""
      msg + suffix
    }
    val result = new ListBuffer[String]
    for (msg <- msgs) {
      val from = msg.timeInfo.from.asInstanceOf[Time]
      val to = msg.timeInfo.to.asInstanceOf[Time]
      if (isRelevant(from, to)) {
        result ++= createTextEntry(msgTxt(msg.info, from, to), fromBar(from), toBar(to), withOffset)
      }
    }
    result
  }

  def renderDayPlanPage(plan: DayPlan, planRightOpt: Option[DayPlan] = None) = {
    val result = new ListBuffer[String]
    result ++= pagePreamble.split("\n").map(_.stripLineEnd)
    //
    val headerA = s"KW${plan.periodEntry.localDate.weekOfWeekyear().get}"
    result += f"\\put(${conf.leftFullHour},${conf.headerPos}){\\scalebox{2}{\\large \\textbf{${headerA}}}}"
    //
    val headerB = s"${plan.periodEntry.localDate.weekyear().get}"
    result += f"\\put(${conf.middle},${conf.headerPos}){\\scalebox{2}{\\makebox[${(conf.right - conf.middle) / 2.0} cm][r]{\\large \\textbf{${headerB}}}}}"
    //
    for ((hour, i) <- (conf.firstHour until conf.lastHour).zipWithIndex) {
      result ++= createHourEntries(hour, i)
      result ++= createHourLines(hour, i)
    }
    val currentBottom = startI(conf.lastHour - conf.firstHour)
    result += f"\\put(${conf.left},${currentBottom}){\\line(1,0){${conf.lineWidth}}}"
    //
    result += "% text entries left"
    result ++= createHeader(plan.period.header, withOffset = false)
    result ++= addTextEntries(plan.period.todo.appointments, withOffset = false)
    //
    planRightOpt match {
      case Some(planRight) =>
        result += "% text entries right"
        result ++= createHeader(planRight.period.header, withOffset = true)
        result ++= addTextEntries(planRight.period.todo.appointments, withOffset = true)
      case _ =>
    }
    //
    result ++= pagePostamble.split("\n").map(_.stripLineEnd)
    result
  }

  val pagePreamble = s"""
% page preamble
\\begin{center}
\\begin{picture}(${conf.pageWidth},${conf.pageHeight})(2.35,0.5)

% Outer frame
% left / right
\\put(0,0){\\line(0,1){${conf.pageHeight}}}
\\put(${conf.pageWidth},0){\\line(0,1){${conf.pageHeight}}}
% bottom / top
\\put(0,0){\\line(1,0){${conf.pageWidth}}}
\\put(0,${conf.pageHeight}){\\line(1,0){${conf.pageWidth}}}
"""

  val pagePostamble = s"""
% page postamble
\\end{picture}
\\end{center}
"""
}
