package de.gellien.timeplanner

import java.io._
import java.util.Properties
import scala.io.Source
import scala.collection.mutable.ListBuffer
import timeplan._
import latex.LatexTimePlan

object Plan {
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
  type OptionMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit = {
    val usage = """Usage: Plan [-d | --debug] -i timeplan.txt todoFileName[s]"""
    val defaultTexoutput = "example.tex"
    println("os: " + System.getProperty("os.name"))
    println("pwd: " + System.getProperty("user.dir"))
    if (args.length == 0) println(usage)

    val options = getOptions(args)
    println("options:")
    options foreach { option => println("  %s -> %s" format (option._1, option._2)) }
    if (options('input).asInstanceOf[List[String]].size == 0) {
      println("Fatal: need File(s) to read ToDo from")
      println(usage)
      sys.exit(1)
    }
    if (!(options contains 'inputDsl)) {
      println("Fatal: No input DSL file specified (via Option -i/--input_dsl)")
      println(usage)
      sys.exit(1)
    }

    val daysPerWeek = if (options contains 'daysPerWeek) options('daysPerWeek).asInstanceOf[Int] else 7
    if (daysPerWeek < 1 || daysPerWeek > 7) {
      println("Fatal: daysPerWeek needs to be between 1 and 7 (including)")
      sys.exit(1)
    }

    val debug = options contains 'debug
    val callPdfLatex = options contains 'pdflatex
    val texoutput = if (options contains 'texoutput) options('texoutput).asInstanceOf[String] else defaultTexoutput
    val inputDsl = options('inputDsl).asInstanceOf[String] // must exist - see above
    val inputFiles = options('input).asInstanceOf[List[String]].reverse // must exist - see above; reverse if sequence matters
    val withSeparator = options contains 'withSeparator
    val withOverview = options contains 'withOverview
    val inputEncoding = latin1
    val outputEncoding = latin1

    val todoList = for {
      line <- readFiles(inputFiles, inputEncoding)
      td <- ToDoDsl.getToDo(line)
    } yield td

    val properties = new Properties()
    properties.load(new FileInputStream("timeplan.properties"))
    val pdflatexFullPath = properties.getProperty(os + ".pdflatex")
    println("Path to pdflatex: >%s<" format pdflatexFullPath)

    val periodPlans = buildPeriodPlans(inputDsl, todoList, inputEncoding, withOverview, daysPerWeek, debug)
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
    val ltp = new LatexTimePlan(periodPlans, withSeparator)
    val latexSource = ltp.render
    val io = new Io(quote, outputEncoding, debug)
    io.saveStringList(texoutput, latexSource)
    if (callPdfLatex) {
      io.callPdfLaTeX(pdflatexFullPath, texoutput)
    }
  }

  def buildPeriodPlans(inputDsl: String, todoList: List[ToDo], inputEncoding: String, withOverview: Boolean, daysPerWeek: Int, debug: Boolean) = {
    for {
      line <- getFilteredLines(inputDsl, inputEncoding)
      pe <- ToDoDsl.getPeriodEntry(line)
      tp = TimePlan(pe)(daysPerWeek)
    } yield tp.createPeriodPlan(todoList, withOverview)
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
