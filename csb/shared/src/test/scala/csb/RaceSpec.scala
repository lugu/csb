package csb

import org.scalatest._

class RaceSpec extends FlatSpec with Matchers {

  val dummyCheckpoints = List(Point(1, 1), Point(2, 2), Point(3, 3), Point(4, 4), Point(5, 5))

  "PodUpdater" should "be able to parse input correctly" in {
    val input = "-1000 2000 200 -100 90 3"
    val updater = PodUpdater(dummyCheckpoints)
    val pod = updater.parsePodUpdate(input).pod

    assert(pod.position == Point(-1000, -2000), "parse position")
    assert(pod.speed == Point(200, 100), "parse speed")
    assert(pod.destinations.head == Point(4, 4), "parse index")

    pod.orientation.x should equal (0.0 +- 0.001)
    pod.orientation.y should equal (-1.0 +- 0.001)
  }

  def updatePosition(before: String, after: String, command: String) = {
    val updater = PodUpdater(dummyCheckpoints)
    val pod = updater.parsePodUpdate(before).pod
    val computed = pod.update(new Command(command))
    val result = updater.parsePodUpdate(after).pod

    assert(computed.position == result.position, "in position test")
  }

  def updateTwoPlayerPosition(input: String) = {
    val lines = input.split("\n")
    updatePosition(lines(0), lines(6), lines(4))
    updatePosition(lines(1), lines(7), lines(5))
  }

  "Pod" should "be able to update their position" in {
    val before = "3576 3719 -43 694 86 3"
    val command = "8689 1866 200 200 SKIP-CORRECTED"
    val after = "3608 4598 27 747 68 3"
    updatePosition(before, after, command)
  }

  "Pod" should "be able to update their position when boosting" in {
    // val input =
    // "3576 3719 -43 694 86 3\n" +
    // "4149 7748 307 381 348 4\n" +
    // "2130 1048 -141 249 74 3\n" +
    // "3187 5395 55 357 40 4\n" +
    // "8689 1866 200 200 SKIP-CORRECTED\n" +
    // "7398 4462 BOOST BOOST BOOST-CORRECTED\n" +
    // "3608 4598 27 747 68 3\n" +
    // "4628 8028 407 237 330 4\n" +
    // "2015 1373 -97 275 71 3\n" +
    // "3316 5782 109 328 22 4\n" +
    // "3358 7097 143 143 PILOT0-CORRECTED\n" +
    // "7226 5234 BOOST BOOST BOOST-CORRECTED\n"
    // updateTwoPlayerPosition(input)
  }
  
  "Pod" should "be able to update their position during collision" in {
    // one before
    // 3576 3719 -43 694 86 3
    // 4149 7748 307 381 348 4
    // 2130 1048 -141 249 74 3
    // 3187 5395 55 357 40 4
    // 8689 1866 200 200 SKIP-CORRECTED
    // 7398 4462 BOOST BOOST BOOST-CORRECTED
    // before:
    // 3608 4598 27 747 68 3
    // 4628 8028 407 237 330 4
    // 2015 1373 -97 275 71 3
    // 3316 5782 109 328 22 4
    // 3358 7097 143 143 PILOT0-CORRECTED
    // 7226 5234 BOOST BOOST BOOST-CORRECTED
    // afer:
    // 3608 4598 27 747 68 3
    // 4628 8028 407 237 330 4
    // 2015 1373 -97 275 71 3
    // 3316 5782 109 328 22 4
    // 3358 7097 143 143 PILOT0-CORRECTED
    // 7226 5234 BOOST BOOST BOOST-CORRECTED
  }
  it should "compute shield collision " in {
   // one before:
   // 7601 10466 -89 538 202 2
   // 8540 5751 -214 727 73 1
   // 7682 7973 249 33 336 4
   // 7367 6221 267 -274 348 4
   // 8761 3056 77 77 PILOT1-CORRECTED
   // 9643 8337 200 200 PILOT0-CORRECTED
   // Before:
   // 7453 10955 -125 415 220 2
   // 8404 6662 -115 774 67 1
   // 8003 7972 273 -1 335 4
   // 7713 5933 293 -244 350 4
   // 9006 3631 98 98 PILOT1-CORRECTED
   // 10403 8915 SHIELD SHIELD DEFENSE-CORRECTED
   // after:
   // 7276 11287 -150 282 238 2
   // 8297 7380 -79 525 49 2
   // 8271 8499 112 1293 333 4
   // 8085 5678 316 -217 352 4
   // 9366 4164 119 119 PILOT1-CORRECTED
   // 8748 -92 60 60 PILOT1-CORRECTED

  }
}

