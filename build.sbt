// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

val sharedSettings = Seq(
 name := "CoderStrikeBack",
 version := "0.1-SNAPSHOT",
 scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val csb =
  crossProject(JSPlatform, JVMPlatform).
    settings(sharedSettings).
    jvmSettings(
      scalaVersion := "2.12.2",
      libraryDependencies ++= Seq(
              "org.scalatest" %% "scalatest" % "3.0.1" % "test",
              "org.scala-lang" % "scala-compiler" % "2.12.2" % "test"
      )
    ).
    jsSettings(
      scalaVersion := "2.11.11",
      // localUrl := ("localhost", 8080),
      libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "0.9.1",
          "com.lihaoyi" %%% "scalatags" % "0.6.2",
          "org.scalactic" %%% "scalactic" % "3.0.1" % "test",
          "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
      )
    )

lazy val csbJS 	   = csb.js.enablePlugins(ScalaJSPlugin) // , WorkbenchPlugin)
lazy val csbJVM    = csb.jvm
