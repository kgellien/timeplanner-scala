package de.gellien.timeplanner.config

import java.io.FileInputStream
import java.util.Properties

sealed abstract class ConfigEntry
case class Modifier(quote: String, daysPerWeek: Int, withSeparator: Boolean, withOverview: Boolean, callPdfLatex: Boolean, debug: Boolean, withAdditionalTasks: Boolean) extends ConfigEntry
case class FileNames(inputFiles: List[String], inputDsl: String, pdflatexFullPath: String, timeplanoutput: String, dayplanoutput: String, dpconfig: String, weekworkplanoutput: String, weekscheduleoutput: String, weekschedule24output: String) extends ConfigEntry
case class Encodings(inputEncoding: String, outputEncoding: String) extends ConfigEntry

object Config {
  type OptionMap = Map[Symbol, Any]

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
    println("java: " + System.getProperty("java.version"))
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
