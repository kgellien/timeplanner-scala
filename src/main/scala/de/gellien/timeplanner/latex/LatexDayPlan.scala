package de.gellien.timeplanner.latex

import de.gellien.timeplanner.config.Encodings

import java.time.temporal.WeekFields
import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan._

class LatexDayPlans {
  def renderDayPlanPage(conf: DayPlanConf, dayPlan: DayPlan, dayPlanRightOpt: Option[DayPlan] = None) = {
    val latexSingleDayPlan = new LatexDayPlan(conf)
    latexSingleDayPlan.render(dayPlan, dayPlanRightOpt)
  }
  def render(plans: List[PeriodPlan], conf: DayPlanConf)(implicit encodings: Encodings) = {
    val result = new ListBuffer[String]
    result ++= LatexHeader.header(DayPlanConf2)
    result += "\\begin{document}"
    result += "\\setlength{\\unitlength}{10mm}"
    for (plan <- plans) {
      plan match {
        case dayPlan: DayPlan => result ++= renderDayPlanPage(conf, dayPlan)
        case weekPlan: WeekPlan =>
          val dayPlans = for (singlePeriod <- weekPlan.periodSpecifics)
            yield DayPlan(singlePeriod.periodEntry.asInstanceOf[DayEntry], singlePeriod, List[SinglePeriod](), false)
          if (weekPlan.daysPerWeek == 7) {
            val (weekDays, weekend) = dayPlans.splitAt(5)
            for (dayPlan <- weekDays)
              result ++= renderDayPlanPage(conf, dayPlan)
            result ++= renderDayPlanPage(conf, weekend(0), Some(weekend(1)))
          } else for (dayPlan <- dayPlans)
            result ++= renderDayPlanPage(conf, dayPlan)
        case _ => //println(s"Ignore ${plan.period}")
      }
    }
    result += """\end{document}"""
  }
}

class LatexDayPlan(conf: DayPlanConf) {
  val weekFields = WeekFields.ISO
  def render(plan: DayPlan, planRightOpt: Option[DayPlan] = None) = {
    val result = new ListBuffer[String]
    result ++= pagePreamble.split("\n").map(_.stripLineEnd)
    //
    val dayHeaderY = conf.headerPos
    val headerA = s"W${plan.periodEntry.localDate.get(weekFields.weekOfWeekBasedYear())}"
    result += f"\\put(${conf.leftFullHour},${dayHeaderY}){\\scalebox{2}{\\large \\textbf{${headerA}}}}"
    val headerB = s"${TimeHelper.getWeekyear(plan.periodEntry.localDate)}"
    result += f"\\put(${conf.middle},${dayHeaderY}){\\scalebox{2}{\\makebox[${(conf.right - conf.middle) / 2.0} cm][r]{\\large \\textbf{${headerB}}}}}"
    val dayHeaderX = if (planRightOpt.isDefined) 2.0 * conf.leftContent + conf.leftFullHour else conf.middle - (conf.middle - conf.leftContent) / 4.0
    result += f"\\put(${dayHeaderX},${dayHeaderY}){\\scalebox{1.25}{\\large \\textbf{${plan.period.header}}}}"
    //
    for ((hour, i) <- (conf.firstHour until conf.lastHour).zipWithIndex) {
      result ++= createHourEntries(hour, i, planRightOpt.isDefined)
    }
    val currentBottom = startI(conf.lastHour - conf.firstHour)
    result += f"\\put(${conf.left},${currentBottom}){\\line(1,0){${conf.lineWidth}}}"
    //
    result += "% text entries left"
    result ++= addTextEntries(plan.period.todo.appointments, withOffset = false)
    //
    planRightOpt match {
      case Some(planRight) =>
        result += "% text entries right"
        result += f"\\put(${conf.middle + conf.leftFullHour},${conf.headerPos}){\\scalebox{1.25}{\\large \\textbf{${planRight.period.header}}}}"
        result ++= addTextEntries(planRight.period.todo.appointments, withOffset = true)
      case _ =>
    }
    //
    result ++= pagePostamble.split("\n").map(_.stripLineEnd)
  }

  def startI(i: Double) = conf.top - i * conf.hourLineDelta

  def createHourEntries(hour: Int, i: Int, withMiddleLine: Boolean) = {
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
    // hour lines
    val startHalf = start - conf.hourLineDelta / 2.0
    result += s"% createHourLines(${hour}, ${i})"
    result += "% vertical hour lines"
    result += f"\\put(${conf.leftContent},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    if (withMiddleLine)
      result += f"\\put(${conf.middle},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += f"\\put(${conf.right},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += "% horizontal hour lines"
    result += f"\\put(${conf.leftContent},${start}){\\line(1,0){${conf.hourLength}}}"
    result += f"\\put(${conf.leftContent},${startHalf}){\\line(1,0){${conf.halfHourLength}}}"
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
  }

  def addTextEntries(msgs: List[Appointment], withOffset: Boolean) = {
    val startTime = Time(conf.firstHour, 0)
    val endTime = Time(conf.lastHour + 1, 0)
    def isRelevant(from: Time, to: Time) = (from.hh >= conf.firstHour && from.hh < conf.lastHour) ||
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
      if (isRelevant(from, to))
        result ++= createTextEntry(msgTxt(msg.info, from, to), fromBar(from), toBar(to), withOffset)
    }
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
