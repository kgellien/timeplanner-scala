package de.gellien.timeplanner.timeplan
import org.specs2.mutable.SpecificationWithJUnit

class AppointmentSpec extends SpecificationWithJUnit {
  val line = "2015-W17 24. -- 26. Aikido seminar in Ffm"
  val seminar = ToDoDsl.getToDo(line).get

  def getSubs(todo: ToDo) = todo match {
    case app @ Appointment(pe, cl, db, ti, info) => app.extractSubTasks()
    case _                                       => Nil
  }

  "Appointments" should {
    "have ???" in {
      getSubs(seminar).size must_== 3
    }

  }
}