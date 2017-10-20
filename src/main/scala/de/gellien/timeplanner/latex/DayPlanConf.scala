package de.gellien.timeplanner.latex

abstract class DayPlanConf {
  val left = 0.0
  val pageWidth = 18.5
  val pageHeight = 27.5
  val bottom = 0.0

  val firstHour: Int
  val hours: Int

  val maxHours: Int
  val hourLineDelta: Double
  val textSize: String
  val textHeight: Double
  val hourTextSize: String
  val leftFullHourWidth: Double
  val leftHalfHourWidth: Double
  val leftContentWidth: Double

  def lineWidth = pageWidth - 2.0 * left
  def headerPos = pageHeight - 1.0
  def leftFullHour = left + leftFullHourWidth
  def leftHalfHour = left + leftHalfHourWidth
  def leftContent = left + leftContentWidth
  def right = pageWidth - left
  def middle = leftContent + (right - leftContent) / 2
  def hourLength = lineWidth - (leftContent - left)
  def halfHourLength = lineWidth - (leftContent - left)
  def top = bottom + maxHours * hourLineDelta
  def lastHour = firstHour + hours

  def defaultDuration = 0.5
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
}

object ConfBig extends DayPlanConf {
  val firstHour = 8
  val hours = 9

  val maxHours = 13
  val hourLineDelta = 2.00
  val textSize = "\\large"
  val textHeight = 0.40
  val hourTextSize = "\\huge"
  val leftFullHourWidth = 0.40
  val leftHalfHourWidth = 1.18
  val leftContentWidth = 2.60
}
