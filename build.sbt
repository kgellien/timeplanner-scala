name := "timeplanner"

version := "0.8.1-SNAPSHOT"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies ++= {
  val specs2Version = "3.9.5"
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    //
    "org.specs2" %% "specs2-core" % specs2Version % "test",
    "org.specs2" %% "specs2-junit" % specs2Version % "test"
  )
}

// activate for HTML-Reports in target/specs2-reports
//testOptions in Test += Tests.Argument("html")
