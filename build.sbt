import sbtcrossproject.{crossProject, CrossType}

val sharedSettings = Seq(
 name := "CoderStrikeBack",
 version := "0.1-SNAPSHOT"
)

lazy val csb =
  crossProject(JSPlatform, JVMPlatform, NativePlatform).
    crossType(CrossType.Full).
    settings(sharedSettings).
    jvmSettings(
      scalaVersion := "2.12.4",
      libraryDependencies ++= Seq(
              "org.scalatest" %% "scalatest" % "3.0.4",
              "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
      )
    ).
    jsSettings(
      scalaVersion := "2.11.12",
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
      // localUrl := ("localhost", 8080),
      libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "0.9.4",
          "com.lihaoyi" %%% "scalatags" % "0.6.7",
          "org.scalactic" %%% "scalactic" % "3.0.4",
          "org.scalatest" %%% "scalatest" % "3.0.4"
      )
    ).
    nativeSettings(
      scalaVersion := "2.11.12",
      nativeGC := "immix"
    )

lazy val csbJS 	   = csb.js.enablePlugins(ScalaJSPlugin)
lazy val csbJVM    = csb.jvm
lazy val csbNative = csb.native.enablePlugins(ScalaNativePlugin)
