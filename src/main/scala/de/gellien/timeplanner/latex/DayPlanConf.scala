package de.gellien.timeplanner.latex

import java.util.Properties
import java.io.FileInputStream

abstract class DayPlanConf {
  val left = 0.0
  val pageWidth = 18.5
  val pageHeight = 27.5
  val bottom = 0.0

  def firstHour: Int
  def hours: Int

  def maxHours: Int
  def hourLineDelta: Double
  def textSize: String
  def textHeight: Double
  def hourTextSize: String
  def leftFullHourWidth: Double
  def leftHalfHourWidth: Double
  def leftContentWidth: Double

  def topFraction: Double
  def top: Double

  def lineWidth = pageWidth - 2.0 * left
  def headerPos = pageHeight - 1.0
  def leftFullHour = left + leftFullHourWidth
  def leftHalfHour = left + leftHalfHourWidth
  def leftContent = left + leftContentWidth
  def right = pageWidth - left
  def middle = leftContent + (right - leftContent) / 2
  def hourLength = lineWidth - (leftContent - left)
  def halfHourLength = lineWidth - (leftContent - left)
  def lastHour = firstHour + hours

  def defaultDuration = 0.5
}

object ConfFactory {
  def getConf(dpConfig: String) = {
    val properties = new Properties()
    properties.load(new FileInputStream(dpConfig))

    new DayPlanConf {
      val firstHour = properties.getProperty("firstHour").toInt
      val hours = properties.getProperty("hours").toInt
      val maxHours = properties.getProperty("maxHours").toInt
      val hourLineDelta = properties.getProperty("hourLineDelta").toDouble
      val textSize = properties.getProperty("textSize")
      val textHeight = properties.getProperty("textHeight").toDouble
      val hourTextSize = properties.getProperty("hourTextSize")
      val leftFullHourWidth = properties.getProperty("leftFullHourWidth").toDouble
      val leftHalfHourWidth = properties.getProperty("leftHalfHourWidth").toDouble
      val leftContentWidth = properties.getProperty("leftContentWidth").toDouble
      val topFraction = properties.getProperty("topFraction").toDouble

      val top = bottom + maxHours * hourLineDelta + hourLineDelta / topFraction
    }
  }
}

object ConfRegular extends DayPlanConf {
  val firstHour = 8
  val hours = 14
  val maxHours = 16
  val hourLineDelta = 1.60
  val textSize = "\\normalsize"
  val textHeight = 0.35
  val hourTextSize = "\\Large"
  val leftFullHourWidth = 0.30
  val leftHalfHourWidth = 0.80
  val leftContentWidth = 1.75
  val topFraction = 3.0

  val top = bottom + maxHours * hourLineDelta + hourLineDelta / topFraction
}
