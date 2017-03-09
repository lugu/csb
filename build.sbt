enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

name := "CoderStrikeBack"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "com.lihaoyi" %%% "scalatags" % "0.6.2",
  "org.scalactic" %%% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
)
