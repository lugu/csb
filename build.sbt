
lazy val root = project.in(file(".")).
  aggregate(csbJS, csbJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val csb = crossProject.in(file(".")).
  settings(
    name := "CoderStrikeBack",
    version := "0.1-SNAPSHOT"
  ).
  jvmSettings(
    scalaVersion := "2.12.2",
    libraryDependencies ++= Seq(
	    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
	    "org.scala-lang" % "scala-compiler" % "2.12.2" % "test"
    )
  ).
  jsSettings(
    scalaVersion := "2.11.8",
    localUrl := ("localhost", 8080),
    libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.1",
        "com.lihaoyi" %%% "scalatags" % "0.6.2",
        "org.scalactic" %%% "scalactic" % "3.0.1" % "test",
        "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
    )
  )
lazy val csbJVM = csb.jvm
lazy val csbJS = csb.js.enablePlugins(ScalaJSPlugin, WorkbenchPlugin)
