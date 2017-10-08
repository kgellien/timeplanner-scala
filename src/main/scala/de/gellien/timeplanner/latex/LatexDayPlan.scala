package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.timeplan._

class ConfA4 {
  val left = 0.0
  val pageWidth = 18.5
  val pageHeight = 27.5
  val bottom = 0.0

  val maxHours = 15
  val hours = 15 // 9
  val firstHour = 8
  val noOfDays = 2
  val hourLineDelta = 1.60
  val textSize = "\\normalsize"
  val textHeight = 0.35
  val hourTextSize = "\\Large"
  val leftFullHourWidth = 0.30
  val leftHalfHourWidth = 0.80
  val leftContentWidth = 1.75

  val lineWidth = pageWidth - 2.0 * left
  val headerPos = pageHeight - 1.0

  val leftFullHour = left + leftFullHourWidth
  val leftHalfHour = left + leftHalfHourWidth
  val leftContent = left + leftContentWidth
  val right = pageWidth - left
  val middle = leftContent + (right - leftContent) / 2
  val hourLength = lineWidth - (leftContent - left)
  val halfHourLength = lineWidth - (leftContent - left)
  val top = bottom + maxHours * hourLineDelta
  val lastHour = firstHour + hours

  val defaultDuration = 0.5
}

class LatexDayPlan(plans: List[DayPlan], conf: ConfA4) {
  val pageWidth = 18.5
  val pageHeight = 27.5

  def render = {
    val result = new ListBuffer[String]
    result += """"""
    result += """\documentclass[a4paper,12pt]{article}"""
    result += "\\usepackage[utf8]{inputenc}"
    result += "%\\usepackage[latin1]{inputenc}"
    //    result += "\\usepackage[german]{babel}"
    //    result += "\\usepackage[T1]{fontenc}"
    //    result += "\\usepackage{times}"
    result += "\\usepackage{graphicx}"
    result ++= preamble.split("\n").map(_.stripLineEnd)
    result += """\begin{document}"""
    result += """\pagestyle{empty}"""
    result += s"""\\setlength{\\unitlength}{10mm}"""
    for (plan <- plans)
      result ++= renderSinglePlan(plan)
    result += """\end{document}"""
    result
  }

  def startI(i: Double) = conf.top - i * conf.hourLineDelta

  def createHourLines(hour: Int, i: Int, withOffset: Boolean) = {
    val result = new ListBuffer[String]
    val offset = if (withOffset) conf.middle - conf.leftContent else 0.0
    val start = startI(i)
    val end = startI(i + 1)
    val startHalf = start - conf.hourLineDelta / 2.0
    val fullHour = start - 0.375 * conf.hourLineDelta
    val halfHour = start - 0.875 * conf.hourLineDelta
    result += s"% createHourLines(${hour}, ${i}, ${withOffset})"
    result += "% vertical hour lines"
    result += f"\\put(${conf.leftContent + offset},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    result += f"\\put(${conf.middle + offset},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    //    if (!conf.shortHourLine)
    result += f"\\put(${conf.right},${end}){\\line(0,1){${conf.hourLineDelta}}}"
    //
    result += "% horizontal hour lines"
    result += f"\\put(${conf.leftContent + offset},${start}){\\line(1,0){${conf.hourLength}}}"
    result += f"\\put(${conf.leftContent + offset},${startHalf}){\\line(1,0){${conf.halfHourLength}}}"
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
    val posX = if (withOffset) conf.leftContent + conf.middle - conf.leftContent else conf.leftContent
    val posY = startI(-0.35)
    val width = conf.middle - conf.leftContent
    result += f"\\put(${posX},${posY}){\\scalebox{1.25}{\\makebox[${width} cm][c]{\\large \\textbf{${header}}}}}"
    result
  }

  def escapeMsg(msg: String) = msg.replace("&", "\\&").replace("%", "\\%")

  def timeDiff(from: Time, to: Time) = {
    //Pre: from < to and both on the same day
    val toT = to.hh * 60 + to.mm
    val fromT = from.hh * 60 + from.mm
    (toT - fromT) / 60
  }

  def createTextEntry(msg: String, from: Time, to: Time) = {
    val result = new ListBuffer[String]
    val offsetY = conf.hourLineDelta * (from.mm / 60)
    val textX = conf.leftContent + conf.leftFullHourWidth
    val i = from.hh - conf.firstHour
    val start = startI(i)
    val textY = start - offsetY - conf.textHeight
    val entry = f"\\put(${textX},${textY}){${conf.textSize} ${escapeMsg(msg)}}"
    result += entry
    val thicknessInCm = 0.15
    val posX = conf.leftContent + thicknessInCm / 2
    val td = timeDiff(from, to)
    val duration = if (Math.abs(td) < 0.01) conf.defaultDuration else td
    val length = conf.hourLineDelta * duration
    val posY = start - offsetY - length
    result += f"\\linethickness{${thicknessInCm} cm}"
    result += f"\\put(${posX},${posY}){\\line(0,1){${length}}}"
    result += f"\\linethickness{${thicknessInCm / 2} cm}"
    result += f"\\put(${posX},${posY}){\\line(1,0){${thicknessInCm}}}"
    result += f"\\put(${posX},${posY + length}){\\line(1,0){${thicknessInCm}}}"
    result += f"\\linethickness{0.01cm}"
    result
  }

  def isRelevant(from: Time) = from.hh >= conf.firstHour && from.hh < conf.lastHour

  def addTextEntries(msgs: List[Appointment]) = {
    println(s"addTextEntries:")
    val result = new ListBuffer[String]
    for (msg <- msgs) {
      println(msg)
      //Appointment(2012-04-19,None,List(),Range(09:00,18:00),JAX in Mainz)
      val from = msg.timeInfo.from.asInstanceOf[Time]
      val to = msg.timeInfo.to.asInstanceOf[Time]
      if (isRelevant(from)) {
        if (to.hh >= conf.lastHour) {
          val endTime = Time(conf.lastHour, 0)
          val msgTxt = s"${msg.info} (-- ${endTime})"
          result ++= createTextEntry(msgTxt, from, endTime)
        } else {
          result ++= createTextEntry(msg.info, from, to)
        }
      }
    }
    result
  }

  def renderSinglePlan(plan: DayPlan) = {
    val result = new ListBuffer[String]
    val withOffset = false
    result ++= pagePreamble.split("\n").map(_.stripLineEnd)
    //
    result ++= createHeader(plan.period.header, withOffset)
    for ((hour, i) <- (conf.firstHour until conf.lastHour).zipWithIndex) {
      result ++= createHourEntries(hour, i)
      result ++= createHourLines(hour, i, withOffset)
    }
    val currentBottom = startI(conf.lastHour - conf.firstHour)
    result += f"\\put(${conf.left},${currentBottom}){\\line(1,0){${conf.lineWidth}}}"
    //
    result += "% text entries"
    result ++= addTextEntries(plan.period.todo.appointments)
    //
    result ++= pagePostamble.split("\n").map(_.stripLineEnd)
    result
  }

  val preamble = s"""
\\setlength{\\headsep}{-3 cm}
\\setlength{\\footskip}{1 cm}
\\setlength{\\textheight}{${pageHeight} cm}
\\setlength{\\textwidth}{${pageWidth} cm}
"""

  val pagePreamble = s"""
% page preamble
\\begin{center}
\\begin{picture}(${pageWidth},${pageHeight})(2.35,0.5)

% Aussenrahmen
% links / rechts
\\put(0,0){\\line(0,1){${pageHeight}}}
\\put(${pageWidth},0){\\line(0,1){${pageHeight}}}
% unten / oben
\\put(0,0){\\line(1,0){${pageWidth}}}
\\put(0,${pageHeight}){\\line(1,0){${pageWidth}}}
"""

  val pagePostamble = s"""
% page postamble
\\end{picture}
\\end{center}
"""
}
