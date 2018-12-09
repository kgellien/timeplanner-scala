package de.gellien.timeplanner.latex

trait LatexConf {
  def landscape: Boolean
  def encoding: String
  def pageWidth: String
  def pageHeight: String
  def oddsidemargin: String
  def evensidemargin: String
  def topmargin: String
}

object DayPlanConf2 extends LatexConf {
  val landscape = false
  val encoding = "latin1"
  val pageWidth = "18.5cm"
  val pageHeight = "27.5cm"
  val oddsidemargin = "3.6cm"
  val evensidemargin = "3.6cm"
  val topmargin = "-0.3cm"
}

object TimePlanConf extends LatexConf {
  val landscape = true
  val encoding = "latin1"
  val pageWidth = "27.7cm"
  val pageHeight = "18.0cm"
  val oddsidemargin = "1.0cm"
  val evensidemargin = "1.0cm"
  val topmargin = "0.0cm"
}

class WeekPlanConf extends LatexConf {
  val landscape = true
  val encoding = "latin1"
  val pageWidth = "26.0cm"
  val pageHeight = "18.0cm"
  val oddsidemargin = "1.78cm"
  val evensidemargin = "1.78cm"
  val topmargin = "0.74cm"
  val daywidthWw = "2.65cm"
  val daywidthWs = "3.05cm"
}
