package csb

import org.scalatest._
import scala.io.Source

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
    val computed = pod.update(new Move(command))
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

  "Judge" should "be able to resolve collision 1" in {
    csb.races.RaceRecord2.game.play
  }

  "Judge" should "be able to resolve collision 2" in {
    csb.races.RaceRecord3.game.play
  }
}

