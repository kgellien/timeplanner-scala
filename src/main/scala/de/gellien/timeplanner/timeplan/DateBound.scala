package de.gellien.timeplanner.timeplan

sealed abstract class DateBound(val pe: PeriodEntry)

case class EqBound(override val pe: PeriodEntry) extends DateBound(pe)

case class NeBound(override val pe: PeriodEntry) extends DateBound(pe)

case class LtBound(override val pe: PeriodEntry) extends DateBound(pe)

case class GtBound(override val pe: PeriodEntry) extends DateBound(pe)

case class LeBound(override val pe: PeriodEntry) extends DateBound(pe)

case class GeBound(override val pe: PeriodEntry) extends DateBound(pe)

