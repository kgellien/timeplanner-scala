package de.gellien.timeplanner.timeplan
import org.specs2.mutable.SpecificationWithJUnit

class AppointmentSpec extends SpecificationWithJUnit {
  val line = "2015-W17 4. -- 6. Aikido seminar in Ffm"
  val lineWithMonth = "2015-W17 24.04. -- 26.04. Aikido seminar in Ffm"
  val lineBetweenMonths = "2015-W18 30.04. -- 1.05. Aikido seminar in Ffm"
  val lineEom = "2015-W22 [Aikido] 30. -- 31. Aikido seminar in Ffm"

  def getSubs(line: String) = {
    val todo = ToDoDsl.getToDo(line).get
    todo match {
      case app @ Appointment(pe, cl, db, ti, info) => app.extractSubTasks()
      case _                                       => Nil
    }
  }

  "Appointments" should {
    "have 3 SubTasks for 24. -- 26." in {
      val subs = getSubs(line)
      subs.size must_== 3
    }

    "have 3 SubTasks for 24.04. -- 26.04." in {
      val subs = getSubs(lineWithMonth)
      subs.size must_== 3
    }

    "have 2 SubTasks for 30.04. -- 1.05." in {
      val subs = getSubs(lineBetweenMonths)
      subs.size must_== 2
    }

    "have 2 SubTasks for 30. -- 31. in May" in {
      val subs = getSubs(lineEom)
      subs.size must_== 2
    }
  }
}