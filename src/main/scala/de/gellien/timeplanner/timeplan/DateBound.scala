package de.gellien.timeplanner.timeplan

import org.joda.time.LocalDate

sealed abstract class DateBound(val base: PeriodEntry) {
  // TODO find better way/names
  val lower = base.lower
  val upper = base.upper
  def valid(pe: PeriodEntry): Boolean = false
}

case class EqBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    (pe.upper <= upper) && (pe.lower >= lower)
  }
}

case class NeBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    (pe.upper < lower) || (pe.lower > upper)
  }
}

case class LtBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    pe.upper < lower
  }
}

case class GtBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    pe.lower > upper
  }
}

case class LeBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    pe.upper <= upper
  }
}

case class GeBound(override val base: PeriodEntry) extends DateBound(base) {
  override def valid(pe: PeriodEntry) = {
    pe.lower >= lower
  }
}

object BoundChecker {
  def withinBounds(basePe: PeriodEntry, dateBounds: List[DateBound]) = {
    val rs = for {
      dateBound <- dateBounds
    } yield dateBound.valid(basePe)
    rs forall { _ == true }
  }
}
