package de.gellien.timeplanner.timeplan

import java.io._
import de.gellien.timeplanner.latex.LatexTimePlan

class Io(quote: String, encoding: String, debug: Boolean) {
  def output(target: String, periodPlans: List[PeriodPlan], withSeparator: Boolean, callPdflatex: Boolean, pdflatexFullPath: String) {
    val ltp = new LatexTimePlan(periodPlans, withSeparator)
    val latexSource = ltp.render
    saveStringList(target, latexSource)
    if (callPdflatex) {
      callPdfLaTeX(pdflatexFullPath, target)
    }
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
                        saveAlways: Boolean = false): Int = {
    val (exitValue, procStdErr, procStdOut) = executeArr(cmd)
    if (saveAlways || exitValue != 0) {
      saveStringList(filePrefix + ".stderr", procStdErr)
      saveStringList(filePrefix + ".stdout", procStdOut)
    }
    exitValue
  }

  def getPdfLaTeXCmdArr(source: String, pdflatexFullPath: String): Array[String] = {
    // TODO: extract output-directory from source!
    val lst = pdflatexFullPath :: "-output-directory=." :: quote + source + quote :: Nil
    lst.toArray
  }

  def callPdfLaTeX(pdflatexFullPath: String, source: String) {
    val cmdArr = getPdfLaTeXCmdArr(source, pdflatexFullPath)
    if (debug) {
      // TODO: extract output-directory from source!
      executeAndSaveArr(cmdArr, filePrefix = "pdflatex", saveAlways = true) // ignore exitValue
    } else {
      executeArr(cmdArr) // ignore exitValue
    }
  }

  def saveStringList(fileName: String, list: Seq[String]) {
    val fos = new FileOutputStream(fileName)
    val osw = new OutputStreamWriter(fos, encoding)
    list foreach { line => osw.write(line + "\n") }
    osw.close()
  }
}
