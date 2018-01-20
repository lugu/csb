# Run
## Build optimized version:

    sbt "~csbJS/fullOptJS"
    firefox ./csb/js/target/scala-2.11/classes/index-opt.html

## Build develepment version:

    sbt "~csbJS/fastOptJS"
    firefox ./csb/js/target/scala-2.11/classes/index-dev.html

## Run test suite:

    sbt "~csbJVM/testQuick"

## Generate target/Player.scala for the online competition:

    sbt "~csbJVM/run-main csb.bundle.BundlerMain csb/shared/src/main/scala/csb/player/Player.scala"

# Project

## Status

    - can display a race in the browser

## TODO
	- refactor the code to enable multiple files (https://github.com/TrueLaurel/CodinGame-Scala-Kit)
	- play against yourself in a native app
	- profile and optimize the code
	- play against yourself in the web browser
