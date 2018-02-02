# Run
## Build optimized version:

    sbt "csbJS/fullOptJS"
    firefox ./csb/js/target/scala-2.11/classes/index-opt.html

## Build develepment version:

    sbt "csbJS/fastOptJS"
    firefox ./csb/js/target/scala-2.11/classes/index-dev.html

## Run test suite:

    sbt "csbJVM/testQuick"

## Run simulation:

    sbt "csbNative/runMain csb.RunSimulation"

## Generate target/Player.scala for the online competition:

    sbt "csbJVM/run-main csb.bundle.BundlerMain csb/shared/src/main/scala/Player.scala"
    mv target/Player.scala .
    sbt scalafmt

# Project

## TODO
    - bug: BetterConfig does not better in the browser

# Resources

    - https://www.scala-js.org/api/scalajs-dom/0.9.0/index.html
    - https://www.w3.org/TR/DOM-Level-3-Events
    - http://scala-js.github.io/scala-js-dom/
