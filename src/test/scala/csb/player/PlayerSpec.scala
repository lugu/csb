package csb.player

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {

  def position = Point(0, 0)
  def speed = Point(100, 0)
  def orientation = Point(1, 0)
  def checkpoints = List(Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0))
  def pod = Pod(position, checkpoints, orientation, speed, true)

  "A Pod destination" should "always be a Point" in {
      assert(pod.destination.isInstanceOf[Point])
  }

  it should "compute its angle to destination" in {
    assert(Pod(position, checkpoints, orientation, speed, true).angleToDest == Degree(0))
    assert(Pod(position, checkpoints, orientation.rotate(Degree(90)), speed, true).angleToDest == Degree(90))
    assert(Pod(position, checkpoints, orientation.rotate(Degree(360-45)), speed, true).angleToDest == Degree(-45))
  }

  it should "compute its speed angle to destination" in {
    assert(Pod(position, checkpoints, orientation, speed, true).speedAngleToDest == Degree(0))
    val angle1 = Pod(position, checkpoints, orientation, speed.rotate(Degree(90)), true).speedAngleToDest.degree
    angle1 should equal (-90.0 +- 0.001)
    val angle2 = Pod(position, checkpoints, orientation, speed.rotate(Degree(360-45)), true).speedAngleToDest.degree
    angle2 should equal (45.0 +- 0.001)
  }

  it should "have a radius of 400 while a checkpoint has a radius of 600" in {
    assert(pod.podRadius == 400)
    assert(pod.checkpointRadius == 600)
  }

  it should "compute accuratly its distance to another pod" in {
    val podA = Pod(Point(0, 0), checkpoints, orientation, speed, true)
    val podB = Pod(Point(1000, 0), checkpoints, orientation, speed, true)
    assert(podA.distanceToPod(podB) == (1000 - 2 * 400))
  }

  it should "compute its next position based on its current speed" in {
    val podA = Pod(position, checkpoints, orientation, Point(0, 0), true)
    assert(pod.update(Point(1, 0), 0).position == (pod.position + pod.speed))
    assert(podA.update(Point(1, 0), 100).position == (pod.position + Point(1, 0).normalize * 100))
    assert(pod.update(Point(1, 0), 100).position == (pod.position + pod.speed + Point(1, 0).normalize * 100))
  }

  it should "always have boost available" in {
    pod.boostAvailable should be (true)
  }
}
