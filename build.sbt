name := "timeplanner"

version := "0.8.2"

scalaVersion := "2.13.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies ++= {
  val specs2Version = "4.8.1"
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    //
    "org.specs2" %% "specs2-core" % specs2Version % "test",
    "org.specs2" %% "specs2-junit" % specs2Version % "test"
  )
}

// activate for HTML-Reports in target/specs2-reports
//testOptions in Test += Tests.Argument("html")
