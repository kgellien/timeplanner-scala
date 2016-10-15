name := "timeplanner"

version := "0.7.7"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
  "org.scalatra.scalate" %% "scalate-core" % "1.7.0" withSources(),
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.6"
)

// activate for HTML-Reports in target/specs2-reports
//testOptions in Test += Tests.Argument("html")


libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.12" % "test",
  "org.pegdown" % "pegdown" % "1.4.2" % "test",
  "junit" % "junit" % "4.11" % "test"
)
