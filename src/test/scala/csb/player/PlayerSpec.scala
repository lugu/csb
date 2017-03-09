package csb.player

import org.scalatest._

class PodSpec extends FlatSpec with Matchers {

  def checkpoints = List(Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0))
  def pod = Pod(Point(0, 0), checkpoints, Point(1, 0), Point(0, 0), true)

  "A Pod" should "always have a destination" in {
    pod.destination should be (checkpoints(0))
  }

  it should "always have boost available" in {
    pod.boostAvailable should be (true)
  }
}
