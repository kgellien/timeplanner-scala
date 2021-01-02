package de.gellien.timeplanner

import scala.io.Source
import scala.collection.mutable.ListBuffer
import config._
import timeplan._
import latex._

object Plan {
  import Config._

  def main(args: Array[String]): Unit = {
    implicit val (encodings, fileNames, modifier) = getConfigsAfterValidation(args)

    val io = new Io(modifier.quote, encodings.outputEncoding, modifier.debug)

    val (allPeriodPlans, periodPlans) = read(fileNames, encodings.inputEncoding, modifier)

    println("periodPlans")
    periodPlans.foreach(p => println(p.header))

    if (periodPlans.size > 0) {
      val tpLatexSource = fileNames.timeplanoutput
      createTexOutput(tpLatexSource, periodPlans, io, modifier.withSeparator)
      if (modifier.callPdfLatex) {
        io.callPdfLaTeX(fileNames.pdflatexFullPath, tpLatexSource)
      }
    }

    val dpLatexSource = fileNames.dayplanoutput
    val dpConfig = fileNames.dpconfig
    createTexOutputForDayPlans(dpLatexSource, allPeriodPlans, io, dpConfig)
    if (modifier.callPdfLatex) {
      io.callPdfLaTeX(fileNames.pdflatexFullPath, dpLatexSource)
    }

    val wwLatexSource = fileNames.weekworkplanoutput
    createTexOutputForWeekWorkPlans(wwLatexSource, allPeriodPlans, io)
    if (modifier.callPdfLatex) {
      io.callPdfLaTeX(fileNames.pdflatexFullPath, wwLatexSource)
    }

    val wsLatexSource = fileNames.weekscheduleoutput
    createTexOutputForWeekSchedule(wsLatexSource, allPeriodPlans, io)
    if (modifier.callPdfLatex) {
      io.callPdfLaTeX(fileNames.pdflatexFullPath, wsLatexSource)
    }

    val ws24LatexSource = fileNames.weekschedule24output
    createTexOutputForWeekSchedule24(ws24LatexSource, allPeriodPlans, io)
    if (modifier.callPdfLatex) {
      io.callPdfLaTeX(fileNames.pdflatexFullPath, ws24LatexSource)
    }
  }

  def read(fileNames: FileNames, inputEncoding: String, modifier: Modifier) = {

    val todoList = for {
      line <- readFiles(fileNames.inputFiles, inputEncoding)
      td <- ToDoDsl.getToDo(line)
    } yield td

    val allPeriodPlans = for {
      line <- getFilteredLines(fileNames.inputDsl, inputEncoding)
      pe <- ToDoDsl.getPeriodEntry(line)
    } yield PeriodPlan(pe, todoList, modifier.withOverview)(modifier.daysPerWeek, modifier.withAdditionalTasks)

    val (dayPlansPp, periodPlans) = allPeriodPlans.partition {
      case dp: DayPlan => true
      case _           => false
    }
    (allPeriodPlans, periodPlans)
  }

  def createTexOutputForWeekSchedule24(outputFileName: String, periodPlans: List[PeriodPlan], io: Io)(implicit encodings: Encodings) = {
    val conf = new WeekPlanConf
    val lws = new LatexWeekSchedules
    val latexSource = lws.render(periodPlans, conf, LatexWeekSchedule24).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutputForWeekSchedule(outputFileName: String, periodPlans: List[PeriodPlan], io: Io)(implicit encodings: Encodings) = {
    val conf = new WeekPlanConf
    val lws = new LatexWeekSchedules
    val latexSource = lws.render(periodPlans, conf, LatexWeekSchedule).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutputForWeekWorkPlans(outputFileName: String, periodPlans: List[PeriodPlan], io: Io)(implicit encodings: Encodings) = {
    val conf = new WeekPlanConf
    val lwwp = new LatexWeekSchedules
    val latexSource = lwwp.render(periodPlans, conf, LatexWeekWorkPlan).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutputForDayPlans(outputFileName: String, periodPlans: List[PeriodPlan], io: Io, dpConfig: String)(implicit encodings: Encodings) = {
    val conf = ConfFactory.getConf(dpConfig)
    val ldp = new LatexDayPlans
    val latexSource = ldp.render(periodPlans, conf).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutput(outputFileName: String, periodPlans: List[PeriodPlan], io: Io, withSeparator: Boolean)(implicit encodings: Encodings) = {
    val ltp = new LatexTimePlan(periodPlans, withSeparator)
    val latexSource = ltp.render.toList
    io.saveStringList(outputFileName, latexSource)
  }

  def readFiles(fileNames: List[String], encoding: String) = {
    val result = new ListBuffer[String]
    for (fileName <- fileNames) {
      val lines = getFilteredLines(fileName, encoding)
      result.appendAll(lines)
      println("Read " + lines.size.toString + " lines from " + fileName)
    }
    result.toList.sortWith((e1, e2) => (e1 compareTo e2) < 0)
  }

  def getFilteredLines(fileName: String, encoding: String) = Source.fromFile(fileName, encoding).getLines().toList.filterNot { line => (line.startsWith("#") || line.isEmpty()) }
}
