package de.gellien.timeplanner.timeplan
import org.specs2.mutable.SpecificationWithJUnit

class AppointmentSpec extends SpecificationWithJUnit {
  val lineW52 = "2016-W52 23. - 27.12. Aikido seminar in Ffm"
  val lineW1 = "2015-W01 4. - 6.1. Aikido seminar in Ffm"
  val line = "2015-W17 4. - 6.04. Aikido seminar in Ffm"
  val lineWithoutToDate = "2015-W17 4.04. Aikido seminar in Ffm"
  val lineWithMonth = "2015-W17 24.04. - 26.04. Aikido seminar in Ffm"
  val lineWithOneMonth = "2015-W17 24. - 26.04. Aikido seminar in Ffm"
  val lineBetweenMonths = "2015-W18 30.04. - 1.05. Aikido seminar in Ffm"
  val lineEom = "2015-W22 [Aikido] 30. - 31.5. Aikido seminar in Ffm"

  def getSubs(line: String) = {
    val todo = ToDoDsl.getToDo(line).get
    todo match {
      case app @ Appointment(pe, cl, db, ti, info) => Appointment.extractSubTasks(app)
      case _                                       => Nil
    }
  }

  "Appointments" should {
    "have 3 SubTasks for 4. - 6." in {
      val subs = getSubs(line)
      subs.size must_== 3
    }

    "have 3 SubTasks for 24.04. - 26.04." in {
      val subs = getSubs(lineWithMonth)
      println(subs)
      subs.size must_== 3
    }

    "have 2 SubTasks for 30.04. - 1.05." in {
      val subs = getSubs(lineBetweenMonths)
      subs.size must_== 2
    }

    "have 2 SubTasks for 30. - 31.5." in {
      val subs = getSubs(lineEom)
      subs.size must_== 2
    }

    "printTimeInfo should work with lineBetweenMonths" in {
      val td = ToDoDsl.getToDo(lineBetweenMonths).get
      td match {
        case a: Appointment =>
          ToDoHelper.printTimeInfo(a)
          val (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = ToDoHelper.getDatespan(a.timeInfo)
          val fromIso = ToDoHelper.toIsoDate(fromDay, fromMonth, fromYear, a.periodEntry.asInstanceOf[PeriodEntry])
          val toIso = ToDoHelper.toIsoDate(toDay, toMonth, toYear, a.periodEntry.asInstanceOf[PeriodEntry])
          fromIso must_== Some(IsoDate(2015, 4, 30))
          toIso must_== Some(IsoDate(2015, 5, 1))
        case _ => println("Appointment expected")
      }
      td.periodEntry.hashCode must_== -402402315
    }

    "printTimeInfo should work with lineW1" in {
      val td = ToDoDsl.getToDo(lineW1).get
      td match {
        case a: Appointment =>
          ToDoHelper.printTimeInfo(a)
          val (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = ToDoHelper.getDatespan(a.timeInfo)
          val fromIso = ToDoHelper.toIsoDate(fromDay, fromMonth, fromYear, a.periodEntry.asInstanceOf[PeriodEntry])
          val toIso = ToDoHelper.toIsoDate(toDay, toMonth, toYear, a.periodEntry.asInstanceOf[PeriodEntry])
          fromIso must_== Some(IsoDate(2015, 1, 4))
          toIso must_== Some(IsoDate(2015, 1, 6))
        case _ => println("Appointment expected")
      }
      td.periodEntry.hashCode must_== 500287211
    }

    "printTimeInfo should work with lineWithOneMonth" in {
      val td = ToDoDsl.getToDo(lineWithOneMonth).get
      td match {
        case a: Appointment =>
          ToDoHelper.printTimeInfo(a)
          val (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = ToDoHelper.getDatespan(a.timeInfo)
          val fromIso = ToDoHelper.toIsoDate(fromDay, fromMonth, fromYear, a.periodEntry.asInstanceOf[PeriodEntry])
          val toIso = ToDoHelper.toIsoDate(toDay, toMonth, toYear, a.periodEntry.asInstanceOf[PeriodEntry])
          fromIso must_== Some(IsoDate(2015, 4, 24))
          toIso must_== Some(IsoDate(2015, 4, 26))
        case _ => println("Appointment expected")
      }
      td.periodEntry.hashCode must_== 134469318
    }

    "printTimeInfo should work with lineW52" in {
      val td = ToDoDsl.getToDo(lineW52).get
      td match {
        case a: Appointment =>
          ToDoHelper.printTimeInfo(a)
          val (fromDay, fromMonth, fromYear, to, toDay, toMonth, toYear) = ToDoHelper.getDatespan(a.timeInfo)
          val fromIso = ToDoHelper.toIsoDate(fromDay, fromMonth, fromYear, a.periodEntry.asInstanceOf[PeriodEntry])
          val toIso = ToDoHelper.toIsoDate(toDay, toMonth, toYear, a.periodEntry.asInstanceOf[PeriodEntry])
          fromIso must_== Some(IsoDate(2016, 12, 23))
          toIso must_== Some(IsoDate(2016, 12, 27))
        case _ => println("Appointment expected")
      }
      td.periodEntry.hashCode must_== -1237712040
    }

    "printTimeInfo should work with lineWithoutToDate" in {
      val td = ToDoDsl.getToDo(lineWithoutToDate).get
      td match {
        case a: Appointment =>
          val (fromIso, toIso) = ToDoHelper.extractTimeInfo(a)
          fromIso must_== Some(IsoDate(2015, 4, 4))
          toIso must_== Some(IsoDate(2015, 4, 4))
        case _ => println("Appointment expected")
      }
      td.periodEntry.hashCode must_== 134469318
    }
  }
}
