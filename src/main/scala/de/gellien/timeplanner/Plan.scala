package de.gellien.timeplanner

import timeplan.{ TimePlan, PeriodPlanDsl, Io }

object Plan {

  val latin1 = "iso-8859-1"
  type OptionMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit = {
    val usage = """Usage: Plan [-d | --debug] todoFileName[s]"""
    val defaultTexoutput = "example.tex"
    println("os: " + System.getProperty("os.name"))
    println("pwd: " + System.getProperty("user.dir"))
    if (args.length == 0) println(usage)

    val options = getOptions(args)
    println(options)
    if (options('input).asInstanceOf[List[String]].size == 0) {
      println("Fatal: need File(s) to read ToDo from")
      println(usage)
      sys.exit(1)
    }

    val debug = options contains 'debug
    val callPdfLatex = options contains 'pdflatex
    val texoutput = if (options contains 'texoutput) options('texoutput).asInstanceOf[String] else defaultTexoutput
    val inputDsl = if (options contains 'inputDsl) Some(options('inputDsl).asInstanceOf[String]) else None
    val inputFiles = options('input).asInstanceOf[List[String]].reverse // must exist - see above; reverse if sequence matters
    val withSeparator = options contains 'withSeparator
    val todoList = Io.readFiles(inputFiles)

    val tp = inputDsl match {
      case Some(fileName) =>
        println("Evaluate %s for construction of TimePlan instance" format fileName)
        buildTimePlan(fileName, todoList, debug)
      case None =>
        println("No input DSL file specified; construct default TimePlan instance")
        defaultTimePlan(todoList, debug)
    }
    Io.output(texoutput, tp, withSeparator, callPdfLatex, debug)
  }

  def buildTimePlan(fileName: String, todoList: List[String], debug: Boolean) = {
    import scala.io.Source
    val lines = Source.fromFile(fileName, latin1).getLines.toList
    val tp = new TimePlan(todoList, debug) // todoList for addPeriodPlan calls not used
    for (line <- lines) {
      PeriodPlanDsl.getPeriodPlan(line, todoList) match {
        case Left(msg) =>
          println("Problem with >%s<" format line)
          println("Left: %s" format msg)
        case Right(optPp) => optPp match {
          case None =>
            println("ignored >%s<" format line)
          case Some(periodPlan) =>
            tp.addPeriodPlan(periodPlan)
            println("parsed  >%s<" format line)
        }
      }
    }
    tp
  }

  // modeled after http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli (pjotrp)
  def getOptions(args: Array[String]): OptionMap = {
    import scala.collection.mutable.{ Map => MMap }
    type OptionMMap = MMap[Symbol, Any]
    def nextOption(map: OptionMMap, list: List[String]): OptionMMap = {
      list match {
        case Nil => map
        case "--input_dsl" :: fileName :: tail => nextOption(map ++ Map('inputDsl -> fileName), tail)
        case "-i" :: fileName :: tail => nextOption(map ++ Map('inputDsl -> fileName), tail)
        case "--texoutput" :: fileName :: tail => nextOption(map ++ Map('texoutput -> fileName), tail)
        case "-o" :: fileName :: tail => nextOption(map ++ Map('texoutput -> fileName), tail)
        case "--debug" :: tail => nextOption(map ++ Map('debug -> true), tail)
        case "-d" :: tail => nextOption(map ++ Map('debug -> true), tail)
        case "--withSeparator" :: tail => nextOption(map ++ Map('withSeparator -> true), tail)
        case "-s" :: tail => nextOption(map ++ Map('withSeparator -> true), tail)
        case "--withOverview" :: tail => nextOption(map ++ Map('withOverview -> true), tail)
        case "-v" :: tail => nextOption(map ++ Map('withOverview -> true), tail)
        case "--callpdflatex" :: tail => nextOption(map ++ Map('pdflatex -> true), tail)
        case "-t" :: tail => nextOption(map ++ Map('pdflatex -> true), tail)
        case string :: tail => map('input) = string :: map('input).asInstanceOf[List[String]]; nextOption(map, tail)
      }
    }
    val initialMap: OptionMMap = MMap('input -> List())
    val input = initialMap('input).asInstanceOf[List[String]]
    nextOption(initialMap, args.toList).toMap // return immutable version
  }

  def defaultTimePlan(todoList: List[String], debug: Boolean) = {
    val tp = new TimePlan(todoList, debug)
    tp.addWeekPlans(2012, 5 to 8)
    tp.addMonthPlans(2012, 2 to 3, false)
    tp.addQuarterPlan(2012, 1, false)
    tp.addYearPlan(2012, false)
    tp
  }
}
