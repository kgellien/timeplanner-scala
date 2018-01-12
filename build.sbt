name := "timeplanner"

version := "0.7.9-SNAPSHOT"

//scalaVersion := "2.11.8"
scalaVersion := "2.12.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies ++= {
  val specs2Version = "3.9.5"
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    "joda-time" % "joda-time" % "2.3",
    "org.joda" % "joda-convert" % "1.6",
    //
    "org.specs2" %% "specs2-core" % specs2Version % "test",
    "org.specs2" %% "specs2-junit" % specs2Version % "test"
  )
}

// activate for HTML-Reports in target/specs2-reports
//testOptions in Test += Tests.Argument("html")
