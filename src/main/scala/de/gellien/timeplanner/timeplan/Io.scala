package de.gellien.timeplanner.timeplan

import java.io._
import scala.collection.mutable.ListBuffer

class Io(quote: String, encoding: String, debug: Boolean) {

  def readOutProcStream(is: InputStream, tagName: String): List[String] = {
    val content = readOutProcStreamNaked(is)
    val startTag = "<" + tagName + ">"
    val endTag = "</" + tagName + ">"
    if (content != Nil) startTag :: content ::: endTag :: Nil
    else Nil
  }

  def readOutProcStreamNaked(is: InputStream): List[String] = {
    val br = new BufferedReader(new InputStreamReader(is));
    var result: List[String] = Nil
    var line: String = br.readLine()
    while (line != null) {
      if (line.trim().size > 0) result = line :: result
      line = br.readLine()
    }
    result.reverse
  }

  def executeArr(cmd: Array[String]): (Int, List[String], List[String]) = {
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

  def executeAndSaveArr(cmd: Array[String], filePrefix: String): Int = {
    val (exitValue, procStdErr, procStdOut) = executeArr(cmd)
    saveStringList(filePrefix + ".stderr", procStdErr)
    saveStringList(filePrefix + ".stdout", procStdOut)
    exitValue
  }

  def callPdfLaTeX(pdflatexFullPath: String, source: String) {
    println("callPdfLaTeX...")
    val outputDir = new File(source).getAbsoluteFile.getParentFile.getCanonicalPath
    val cmd = pdflatexFullPath :: ("-output-directory=%s" format outputDir) :: quote + source + quote :: Nil
    executeAndSaveArr(cmd.toArray, filePrefix = "%s/pdflatex" format outputDir) // ignore exitValue
  }

  def saveString(fileName: String, string: String) {
    val fos = new FileOutputStream(fileName)
    val osw = new OutputStreamWriter(fos, encoding)
    osw.write(string + "\n")
    osw.close()
  }

  def saveStringList(fileName: String, list: Seq[String]) {
    val fos = new FileOutputStream(fileName)
    val osw = new OutputStreamWriter(fos, encoding)
    list foreach { line => osw.write(line + "\n") }
    osw.close()
  }
}
