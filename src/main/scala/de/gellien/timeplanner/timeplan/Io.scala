package de.gellien.timeplanner.timeplan

import java.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import de.gellien.timeplanner.latex.LatexTimePlan
import java.util.Properties

object Io {

  val winQuote = "\"" // WinXP
  val macQuote = "" // Mac OS X
  val linQuote = "" // Linux
  val osName = System.getProperty("os.name")
  val quote = osName match {
    case "Linux" => linQuote
    case "Mac OS X" => macQuote
    case "Windows XP" => winQuote
    case "Windows 7" => winQuote
    case _ =>
      println("WARNING: osName >%s< not yet recognized; take winQuote" format osName)
      winQuote
  }

  def saveStringList(fileName: String, list: Seq[String], encoding: String = "iso-8859-1") {
    val fos = new FileOutputStream(fileName)
    val osw = new OutputStreamWriter(fos, encoding)
    list foreach { line => osw.write(line + "\n") }
    osw.close()
  }

  def readOutProcStream(is: InputStream, tagName: String): List[String] = {
    val content = readOutProcStreamNaked(is)
    val startTag = "<" + tagName + ">"
    val endTag = "</" + tagName + ">"
    if (content != Nil) startTag :: content ::: endTag :: Nil
    else Nil
  }

  def readOutProcStreamNaked(is: InputStream): List[String] = {
    val isr = new InputStreamReader(is);
    val br = new BufferedReader(isr);
    var result: List[String] = Nil
    var line: String = br.readLine()
    while (line != null) {
      if (line.trim().size > 0) result = line :: result
      line = br.readLine()
    }
    result.reverse
  }

  def executeArr(cmd: Array[String]): (Int, List[String], List[String]) = {
    //cmd foreach println
    val proc = Runtime.getRuntime.exec(cmd)
    val procStdErr = readOutProcStream(proc.getErrorStream, "ERROR")
    // procStdIn is *Output* of proc, but input for this process
    val procStdIn = readOutProcStream(proc.getInputStream, "OUTPUT")
    proc.getErrorStream.close()
    proc.getInputStream.close()
    proc.getOutputStream.close()
    val exitValue = proc.waitFor()
    println("execution finished")
    (exitValue, procStdErr, procStdIn)
  }

  def executeAndSaveArr(cmd: Array[String], filePrefix: String,
    saveAlways: Boolean = false, encoding: String = "iso-8859-1"): Int = {
    val (exitValue, procStdErr, procStdOut) = executeArr(cmd)
    if (saveAlways || exitValue != 0) {
      saveStringList(filePrefix + ".stderr", procStdErr, encoding)
      saveStringList(filePrefix + ".stdout", procStdOut, encoding)
    }
    exitValue
  }

  def getPdfLaTeXCmdArr(source: String): Array[String] = {
    val pdflatex = osName match {
      case "Linux" => "/usr/bin/pdflatex"
      case "Mac OS X" => "/usr/texbin/pdflatex"
      case "Windows XP" => "pdflatex"
      case "Windows 7" => "c:/Program Files (x86)/MiKTeX 2.9/miktex/bin/pdfplatex.exe"
      case _ =>
        println("WARNING: osName >%s< not yet recognized; take >pdflatex<" format osName)
        "pdflatex"
    }
    //
    // Read properties file.
    // TODO: make property file optional, i.e. check existence and if non-existent use above defaults
    val properties = new Properties()
    properties.load(new FileInputStream("timeplan.properties"))
    val pdflatexFullPath = properties.getProperty("pdflatex")
    println("Path to pdflatex: >%s<" format pdflatexFullPath)
    // TODO: extract output-directory from source!
    val lst = pdflatexFullPath :: "-output-directory=." :: quote + source + quote :: Nil
    lst.toArray
  }

  def callPdfLaTeX(source: String, debug: Boolean) {
    val cmdArr = getPdfLaTeXCmdArr(source)
    if (debug) {
      // TODO: extract output-directory from source!
      executeAndSaveArr(cmdArr, filePrefix = "pdflatex", saveAlways = true) // ignore exitValue
    } else {
      executeArr(cmdArr) // ignore exitValue
    }
  }

  def output(target: String, tp: TimePlan, withSeparator: Boolean, callPdflatex: Boolean, debug: Boolean = false) {
    val ltp = new LatexTimePlan(tp, withSeparator)
    val latexSource = ltp.render
    printToFile(new File(target))(pw => {
      latexSource foreach pw.println
    })
    if (callPdflatex)
      callPdfLaTeX(target, debug)
  }

  def readFiles(fileNames: List[String], debug: Boolean = false) = {
    val result = new ListBuffer[String]
    for (fileName <- fileNames) {
      val lines = Source.fromFile(fileName, "iso-8859-1").getLines.toList
      result.appendAll(lines)
      //if (debug)
      println("Read " + lines.size.toString + " lines from " + fileName)
    }
    result.toList.sortWith((e1, e2) => (e1 compareTo e2) < 0)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val pw = new java.io.PrintWriter(f, "ISO8859_1")
    try { op(pw) } finally { pw.close() }
  }
}