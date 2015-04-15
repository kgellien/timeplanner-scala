package de.gellien.timeplanner

import java.io._
import java.util.Properties
import scala.io.Source
import scala.collection.mutable.ListBuffer
import timeplan._
import latex.LatexTimePlan

object Plan {
  type OptionMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit = {
    val (encodings, fileNames, modifier) = getConfigsAfterValidation(args)

    val todoList = for {
      line <- readFiles(fileNames.inputFiles, encodings.inputEncoding)
      td <- ToDoDsl.getToDo(line)
    } yield td

    val periodPlans = for {
      line <- getFilteredLines(fileNames.inputDsl, encodings.inputEncoding)
      pe <- ToDoDsl.getPeriodEntry(line)
    } yield PeriodPlan(pe, todoList, modifier.withOverview)(modifier.daysPerWeek)

    //    val parms = Map("periodPlans" -> periodPlans)
    //    import org.fusesource.scalate._
    //    val engine = new TemplateEngine
    //    val output = engine.layout("timeplan.tex.ssp", parms)
    //    def saveString(fileName: String, string: String) {
    //      val fos = new FileOutputStream(fileName)
    //      val osw = new OutputStreamWriter(fos, outputEncoding)
    //      osw.write(string + "\n")
    //      osw.close()
    //    }
    //    io.saveString("mytimeplan.tex", output)
    val ltp = new LatexTimePlan(periodPlans, modifier.withSeparator)
    val latexSource = ltp.render
    val io = new Io(modifier.quote, encodings.outputEncoding, modifier.debug)
    io.saveStringList(fileNames.texoutput, latexSource)
    if (modifier.callPdfLatex) {
      io.callPdfLaTeX(fileNames.pdflatexFullPath, fileNames.texoutput)
    }
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
    def fatal(msg: String) {
      println("Fatal: %s" format msg)
      println(usage)
      sys.exit(1)
    }
    val winQuote = "\"" // WinXP
    val macQuote = "" // Mac OS X
    val linQuote = "" // Linux
    val osName = System.getProperty("os.name")
    // TODO: use constants for os?
    val (os, quote) = osName match {
      case "Linux"      => ("Linux", linQuote)
      case "Mac OS X"   => ("Mac", macQuote)
      case "Windows XP" => ("WinXP", winQuote)
      case "Windows 7"  => ("Win7", winQuote)
      case _ =>
        println("WARNING: osName >%s< not yet recognized; take >Linux<" format osName)
        ("Linux", linQuote)
    }
    val latin1 = "iso-8859-1"
    val defaultTexoutput = "example.tex"
    println("os: " + System.getProperty("os.name"))
    println("pwd: " + System.getProperty("user.dir"))
    if (args.length == 0) {
      fatal("Arguments needed")
    }
    val options = getOptions(args)
    println("options:")
    options foreach { option => println("  %s -> %s" format (option._1, option._2)) }
    if (options('input).asInstanceOf[List[String]].size == 0) {
      fatal("Need File(s) to read ToDo from")
    }
    if (!(options contains 'inputDsl)) {
      fatal("No input DSL file specified (via Option -i/--input_dsl)")
    }

    val daysPerWeek = if (options contains 'daysPerWeek) options('daysPerWeek).asInstanceOf[Int] else 7
    if (daysPerWeek < 1 || daysPerWeek > 7) {
      fatal("daysPerWeek needs to be between 1 and 7 (including)")
    }

    val debug = options contains 'debug
    val callPdfLatex = options contains 'pdflatex
    val texoutput = if (options contains 'texoutput) options('texoutput).asInstanceOf[String] else defaultTexoutput
    val inputDsl = options('inputDsl).asInstanceOf[String] // must exist - see above
    val inputFiles = options('input).asInstanceOf[List[String]].reverse // must exist - see above; reverse in case sequence matters
    val withSeparator = options contains 'withSeparator
    val withOverview = options contains 'withOverview
    val inputEncoding = latin1
    val outputEncoding = latin1

    val properties = new Properties()
    properties.load(new FileInputStream("timeplan.properties"))
    val pdflatexFullPath = properties.getProperty(os + ".pdflatex")
    println("Path to pdflatex: >%s<" format pdflatexFullPath)

    (Encodings(inputEncoding, outputEncoding),
      FileNames(inputFiles, inputDsl, pdflatexFullPath, texoutput),
      Modifier(quote, daysPerWeek, withSeparator, withOverview, callPdfLatex, debug))
  }

  // modeled after http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli (pjotrp)
  def getOptions(args: Array[String]): OptionMap = {
    import scala.collection.mutable.{ Map => MMap }
    type OptionMMap = MMap[Symbol, Any]
    def nextOption(map: OptionMMap, list: List[String]): OptionMMap = {
      list match {
        case Nil                               => map
        case "--daysPerWeek" :: dpw :: tail    => nextOption(map ++ Map('daysPerWeek -> dpw.toInt), tail)
        case "--input_dsl" :: fileName :: tail => nextOption(map ++ Map('inputDsl -> fileName), tail)
        case "-i" :: fileName :: tail          => nextOption(map ++ Map('inputDsl -> fileName), tail)
        case "--texoutput" :: fileName :: tail => nextOption(map ++ Map('texoutput -> fileName), tail)
        case "-o" :: fileName :: tail          => nextOption(map ++ Map('texoutput -> fileName), tail)
        case "--debug" :: tail                 => nextOption(map ++ Map('debug -> true), tail)
        case "-d" :: tail                      => nextOption(map ++ Map('debug -> true), tail)
        case "--withSeparator" :: tail         => nextOption(map ++ Map('withSeparator -> true), tail)
        case "-s" :: tail                      => nextOption(map ++ Map('withSeparator -> true), tail)
        case "--withOverview" :: tail          => nextOption(map ++ Map('withOverview -> true), tail)
        case "-v" :: tail                      => nextOption(map ++ Map('withOverview -> true), tail)
        case "--callpdflatex" :: tail          => nextOption(map ++ Map('pdflatex -> true), tail)
        case "-t" :: tail                      => nextOption(map ++ Map('pdflatex -> true), tail)
        case string :: tail                    => map('input) = string :: map('input).asInstanceOf[List[String]]; nextOption(map, tail)
      }
    }
    val initialMap: OptionMMap = MMap('input -> List())
    val input = initialMap('input).asInstanceOf[List[String]]
    nextOption(initialMap, args.toList).toMap // return immutable version
  }
}

sealed abstract class Config
case class Modifier(quote: String, daysPerWeek: Int, withSeparator: Boolean, withOverview: Boolean, callPdfLatex: Boolean, debug: Boolean) extends Config
case class FileNames(inputFiles: List[String], inputDsl: String, pdflatexFullPath: String, texoutput: String) extends Config
case class Encodings(inputEncoding: String, outputEncoding: String) extends Config

