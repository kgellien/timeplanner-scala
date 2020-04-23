package de.gellien.timeplanner

import java.io._
import java.util.Properties
import scala.io.Source
import scala.collection.mutable.ListBuffer
import timeplan._
import latex._

object Plan {
  type OptionMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit = {
    val (encodings, fileNames, modifier) = getConfigsAfterValidation(args)

    val io = new Io(modifier.quote, encodings.outputEncoding, modifier.debug)

    val todoList = for {
      line <- readFiles(fileNames.inputFiles, encodings.inputEncoding)
      td <- ToDoDsl.getToDo(line)
    } yield td

    val allPeriodPlans = for {
      line <- getFilteredLines(fileNames.inputDsl, encodings.inputEncoding)
      pe <- ToDoDsl.getPeriodEntry(line)
    } yield PeriodPlan(pe, todoList, modifier.withOverview)(modifier.daysPerWeek, modifier.withAdditionalTasks)

    val (dayPlansPp, periodPlans) = allPeriodPlans.partition {
      case dp: DayPlan => true
      case _           => false
    }

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

  def createTexOutputForWeekSchedule24(outputFileName: String, periodPlans: List[PeriodPlan], io: Io) = {
    val conf = new WeekPlanConf
    val lws = new LatexWeekSchedules
    val latexSource = lws.render(periodPlans, conf, LatexWeekSchedule24).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutputForWeekSchedule(outputFileName: String, periodPlans: List[PeriodPlan], io: Io) = {
    val conf = new WeekPlanConf
    val lws = new LatexWeekSchedules
    val latexSource = lws.render(periodPlans, conf, LatexWeekSchedule).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutputForWeekWorkPlans(outputFileName: String, periodPlans: List[PeriodPlan], io: Io) = {
    val conf = new WeekPlanConf
    val lwwp = new LatexWeekSchedules
    val latexSource = lwwp.render(periodPlans, conf, LatexWeekWorkPlan).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutputForDayPlans(outputFileName: String, periodPlans: List[PeriodPlan], io: Io, dpConfig: String) = {
    val conf = ConfFactory.getConf(dpConfig)
    val ldp = new LatexDayPlans
    val latexSource = ldp.render(periodPlans, conf).toList
    io.saveStringList(outputFileName, latexSource)
  }

  def createTexOutput(outputFileName: String, periodPlans: List[PeriodPlan], io: Io, withSeparator: Boolean) = {
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

  def getFilteredLines(fileName: String, encoding: String) = Source.fromFile(fileName, encoding).getLines.toList.filterNot { line => (line.startsWith("#") || line.isEmpty()) }

  def getConfigsAfterValidation(args: Array[String]) = {
    // TODO: Sort properly;try to make clean distinction between validation and output
    val usage = """Usage: Plan [-d | --debug] -i timeplan.txt todoFileName[s]"""
    def fatal(msg: String): Unit = {
      println("Fatal: %s" format msg)
      println(usage)
      sys.exit(1)
    }
    val winQuote = "\"" // Windows
    val macQuote = "" // Mac OS X
    val linQuote = "" // Linux
    val osName = System.getProperty("os.name")
    // TODO: use constants for os?
    val (os, quote) = osName match {
      case "Linux"      => ("Linux", linQuote)
      case "Mac OS X"   => ("Mac", macQuote)
      case "Windows 7"  => ("Windows", winQuote)
      case "Windows 10"  => ("Windows", winQuote)
      case _ =>
        println("WARNING: osName >%s< not yet recognized; take >Windows<" format osName)
        ("Windows", winQuote)
    }
    val defaultTimeplanoutput = "example.tex"
    val defaultDayplanoutput = "example-dp.tex"
    val defaultDpconfig = "dayplan-regular.properties"
    val defaultWeekworkplanoutput = "example-ww.tex"
    println("os: " + System.getProperty("os.name"))
    println("pwd: " + System.getProperty("user.dir"))
    if (args.length == 0) {
      fatal("Arguments needed")
    }
    val options = getOptions(args)
    println("options:")
    options foreach { option => println("  %s -> %s" format (option._1, option._2)) }
    if (options(Symbol("input")).asInstanceOf[List[String]].size == 0) {
      fatal("Need File(s) to read ToDo from")
    }
    if (!(options contains Symbol("inputDsl"))) {
      fatal("No input DSL file specified (via Option -i/--input_dsl)")
    }

    val daysPerWeek = if (options contains Symbol("daysPerWeek")) options(Symbol("daysPerWeek")).asInstanceOf[Int] else 7
    if (daysPerWeek < 1 || daysPerWeek > 7) {
      fatal("daysPerWeek needs to be between 1 and 7 (including)")
    }

    val debug = options contains Symbol("debug")
    val callPdfLatex = options contains Symbol("pdflatex")
    val timeplanoutput = if (options contains Symbol("timeplanoutput")) options(Symbol("timeplanoutput")).asInstanceOf[String] else defaultTimeplanoutput
    val dayplanoutput = if (options contains Symbol("dayplanoutput")) options(Symbol("dayplanoutput")).asInstanceOf[String] else defaultDayplanoutput
    val weekworkplanoutput = if (options contains Symbol("weekworkplanoutput")) options(Symbol("weekworkplanoutput")).asInstanceOf[String] else defaultWeekworkplanoutput
    val weekscheduleoutput = if (options contains Symbol("weekscheduleoutput")) options(Symbol("weekscheduleoutput")).asInstanceOf[String] else defaultWeekworkplanoutput
    val weekschedule24output = if (options contains Symbol("weekschedule24output")) options(Symbol("weekschedule24output")).asInstanceOf[String] else defaultWeekworkplanoutput
    val inputDsl = options(Symbol("inputDsl")).asInstanceOf[String] // must exist - see above
    val inputFiles = options(Symbol("input")).asInstanceOf[List[String]].reverse // must exist - see above; reverse in case sequence matters
    val withSeparator = options contains Symbol("withSeparator")
    val withOverview = options contains Symbol("withOverview")
    val withAdditionalTasks = options contains Symbol("withAdditionalTasks")
    val dpconfig = if (options contains Symbol("dpconfig")) options(Symbol("dpconfig")).asInstanceOf[String] else defaultDpconfig

    val properties = new Properties()
    properties.load(new FileInputStream("timeplan.properties"))
    val pdflatexFullPath = properties.getProperty(os + ".pdflatex")
    println("Path to pdflatex: >%s<" format pdflatexFullPath)
    val inputEncoding = properties.getProperty("input.encoding")
    val outputEncoding = properties.getProperty("output.encoding")

    import java.util.Locale
    val language = properties.getProperty("locale.language")
    Locale.setDefault(new Locale(language))

    (
      Encodings(inputEncoding, outputEncoding),
      FileNames(inputFiles, inputDsl, pdflatexFullPath, timeplanoutput, dayplanoutput, dpconfig, weekworkplanoutput, weekscheduleoutput, weekschedule24output),
      Modifier(quote, daysPerWeek, withSeparator, withOverview, callPdfLatex, debug, withAdditionalTasks))
  }

  // modeled after http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli (pjotrp)
  def getOptions(args: Array[String]): OptionMap = {
    import scala.collection.mutable.{ Map => MMap }
    type OptionMMap = MMap[Symbol, Any]
    def nextOption(map: OptionMMap, list: List[String]): OptionMMap = {
      list match {
        case Nil => map
        case "--daysPerWeek" :: dpw :: tail => nextOption(map ++ Map(Symbol("daysPerWeek") -> dpw.toInt), tail)
        case "--input_dsl" :: fileName :: tail => nextOption(map ++ Map(Symbol("inputDsl") -> fileName), tail)
        case "-i" :: fileName :: tail => nextOption(map ++ Map(Symbol("inputDsl") -> fileName), tail)
        case "--timeplanoutput" :: fileName :: tail => nextOption(map ++ Map(Symbol("timeplanoutput") -> fileName), tail)
        case "-o" :: fileName :: tail => nextOption(map ++ Map(Symbol("timeplanoutput") -> fileName), tail)
        case "--dayplanoutput" :: fileName :: tail => nextOption(map ++ Map(Symbol("dayplanoutput") -> fileName), tail)
        case "--dpconfig" :: fileName :: tail => nextOption(map ++ Map(Symbol("dpconfig") -> fileName), tail)
        case "--weekworkplanoutput" :: fileName :: tail => nextOption(map ++ Map(Symbol("weekworkplanoutput") -> fileName), tail)
        case "--weekscheduleoutput" :: fileName :: tail => nextOption(map ++ Map(Symbol("weekscheduleoutput") -> fileName), tail)
        case "--weekschedule24output" :: fileName :: tail => nextOption(map ++ Map(Symbol("weekschedule24output") -> fileName), tail)
        case "--debug" :: tail => nextOption(map ++ Map(Symbol("debug") -> true), tail)
        case "-d" :: tail => nextOption(map ++ Map(Symbol("debug") -> true), tail)
        case "--withSeparator" :: tail => nextOption(map ++ Map(Symbol("withSeparator") -> true), tail)
        case "--withAdditionalTasks" :: tail => nextOption(map ++ Map(Symbol("withAdditionalTasks") -> true), tail)
        case "-s" :: tail => nextOption(map ++ Map(Symbol("withSeparator") -> true), tail)
        case "--withOverview" :: tail => nextOption(map ++ Map(Symbol("withOverview") -> true), tail)
        case "-v" :: tail => nextOption(map ++ Map(Symbol("withOverview") -> true), tail)
        case "--callpdflatex" :: tail => nextOption(map ++ Map(Symbol("pdflatex") -> true), tail)
        case "-t" :: tail => nextOption(map ++ Map(Symbol("pdflatex") -> true), tail)
        case string :: tail => map(Symbol("input")) = string :: map(Symbol("input")).asInstanceOf[List[String]]; nextOption(map, tail)
      }
    }
    val initialMap: OptionMMap = MMap(Symbol("input") -> List())
    val input = initialMap(Symbol("input")).asInstanceOf[List[String]]
    nextOption(initialMap, args.toList).toMap // return immutable version
  }
}

sealed abstract class Config
case class Modifier(quote: String, daysPerWeek: Int, withSeparator: Boolean, withOverview: Boolean, callPdfLatex: Boolean, debug: Boolean, withAdditionalTasks: Boolean) extends Config
case class FileNames(inputFiles: List[String], inputDsl: String, pdflatexFullPath: String, timeplanoutput: String, dayplanoutput: String, dpconfig: String, weekworkplanoutput: String, weekscheduleoutput: String, weekschedule24output: String) extends Config
case class Encodings(inputEncoding: String, outputEncoding: String) extends Config
