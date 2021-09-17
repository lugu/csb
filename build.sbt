import sbtcrossproject.{crossProject, CrossType}

val sharedSettings = Seq(
 name := "CoderStrikeBack",
 version := "0.1-SNAPSHOT",
 libraryDependencies ++= Seq(
     "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
     )
)

lazy val csb =
  crossProject(JSPlatform, JVMPlatform, NativePlatform).
    crossType(CrossType.Full).
    settings(sharedSettings).
    jvmSettings(
      scalaVersion := "2.12.14",
      libraryDependencies ++= Seq(
              "org.scalatest" %% "scalatest" % "3.2.9"
      )
    ).
    jsSettings(
      scalaVersion := "2.12.14",
      //scalaVersion := "2.12.9",
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
      // localUrl := ("localhost", 8080),
      libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "1.1.0",
          "com.lihaoyi" %%% "scalatags" % "0.9.4",
          "org.scalactic" %%% "scalactic" % "3.2.9",
          "org.scalatest" %%% "scalatest" % "3.2.9"
      )
    ).
    nativeSettings(
      scalaVersion := "2.12.14",
      scalaVersion := "2.11.12",
      nativeGC := "immix"
    )

lazy val csbJS 	   = csb.js.enablePlugins(ScalaJSPlugin)
lazy val csbJVM    = csb.jvm
lazy val csbNative = csb.native.enablePlugins(ScalaNativePlugin)
