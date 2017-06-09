package csb.player

import math.{ Pi }

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {

  def position = Point(0, 0)
  def speed = Point(100, 0)
  def orientation = Point(1, 0)
  def checkpoints = List(Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0))
  def pod = Pod(position, checkpoints, orientation, speed, true)

  "Pod" should "always be a Point" in {
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
    val command = Command(Point(1, 0), 0, "test")
    val commandA = Command(Point(1, 0), 100, "test")
    assert(pod.update(command).position == (pod.position + pod.speed))
    assert(podA.update(commandA).position == (pod.position + Point(1, 0).normalize * 100))
    assert(pod.update(commandA).position == (pod.position + pod.speed + Point(1, 0).normalize * 100))
  }

  it should "always have boost available" in {
    pod.boostAvailable should be (true)
  }


  "Angle" should "be convertible between degree and gradiant" in {
    assert(Degree(90) == Radian(Pi / 2))
    assert(Degree(180) == Radian(Pi))
    assert(Degree(-180) == Radian(Pi))
    assert(Degree(360) == Radian(0))
  }

  it should "support basic arithmetic" in {
    assert(Radian(-Pi) == -Radian(Pi))
    assert(Radian(2 * Pi) == Radian(0))
    assert(Radian(-2 * Pi) == Radian(0))
    assert(Radian(0.5 - 2 * Pi) == Radian(0.5))
    assert(Radian(0.5 + 2 * Pi) == Radian(0.5))
    assert(Radian(Pi) + Radian(Pi + 0.5) == Radian(0.5))
    assert(Radian(Pi) - Radian(Pi + 0.5) == Radian(-0.5))
  }

  it should "compared in absolute value" in {
    assert(Radian(Pi) > Radian(Pi + 0.5))
    assert(Radian(0) < Radian(0.5))
    assert(Radian(-Pi / 4) < Radian(-Pi / 2))
  }

  "Point" should "support basic arithmetic" in {
    assert(Point(0, 1) + Point(1, 2) == Point(1, 3))
    assert(Point(0, 1) - Point(1, 2) == Point(-1, -1))
    assert(Point(-1, -2) == -Point(1, 2))
    assert(Point(1, 2) * 3 == Point(3, 6))
    assert(Point(0, 1).scalar(Point(1, 0)) == 0.0)
    assert(Point(1, 0).scalar(Point(1, 0)) == 1.0)
  }

  it should "measure distance to other" in {
    assert(Point(0, 0).distanceTo(Point(1, 0)) == 1.0)
    assert(Point(0, 3).normalize == Point(0, 1))
    assert(Point(0, 2).norm == 2.0)
    assert(Point(1, 0).radianFrom(Point(0, 1)) > Radian(1.570))
    assert(Point(1, 0).radianFrom(Point(0, 1)) < Radian(1.5708))
    assert(Point(0, 1).rotate(Point(0, 1).radianFrom(Point(1, 0))).x >= 0.99)
    assert(Point(0, 1).rotate(Point(0, 1).radianFrom(Point(1, 0))).x <= 1.01)
    assert(Point(0, 1).rotate(Point(0, 1).radianFrom(Point(1, 0))).y >= -0.01)
    assert(Point(0, 1).rotate(Point(0, 1).radianFrom(Point(1, 0))).y <= 0.01)
  }

  "Pod object" should "comute distance to line" in {
    assert(Pod.distanceToLine(Point(0, 0), Point(1, 1), Point(3, 3)) == 0)
    assert(Pod.distanceToLine(Point(1, 1), Point(2, 4), Point(3, 5)) == 0)
    assert(Pod.distanceToLine(Point(1, 1), Point(2, 4), Point(3, 6)) <= 1)
    assert(Pod.distanceToLine(Point(1, 1), Point(2, 4), Point(2, 6)) >= 0.5)
    assert(Pod.distanceToLine(Point(1, 1), Point(2, 4), Point(21, 41)) == 0)
    assert(Pod.distanceToLine(Point(1, 1), Point(2, 4), Point(21, 42)) <= 1)
  }

  "Race" should "be able to manage its checkpoints" in {

    val checkpoints = List(Point(0, 0), Point(1, 1) * 1000, Point(2, 2) * 1000, Point(3, 3) * 1000)
    val pods = Pod(Point(0, 0), List(Point(0, 0)), Point(0, 0), Point(0, 0), true)
    val race = Race(List(pod, pod, pod, pod), checkpoints, 1)

    assert(!race.checkpoints.isEmpty)
    assert(race.nextCheckpoint(checkpoints(0)) == checkpoints(1))
    assert(race.nextCheckpoint(checkpoints(3)) == checkpoints(0))
    assert(race.checkpointIndex(checkpoints(3)) == 3)
    assert(race.checkpointIndex(checkpoints(0)) == 0)
  }

  "RaceRecord" should "generate a Race at any step" in {
    val checkpoints = List(Point(0, 0), Point(1, 1) * 1000, Point(2, 2) * 1000, Point(3, 3) * 1000)
    val pods = Pod(Point(0, 0), List(Point(0, 0)), Point(0, 0), Point(0, 0), true)
    val recorded = RaceRecord(1, checkpoints, List())
  }

  "RaceRecord" should "print itself correctly" in {

    val recorder = RaceRecord(3, List(Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255)),
          List(List(Record(Pod(Point(6078,-2770), List(Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255)),
          Point(-0.08214283837133374,-0.9966205667676646), Point(-5,-57), true),
          Some(Command(Point(3657.6090240582307,-4526.499598349398),138.57104500478908,"PILOT1-CORRECTED"))),
          Record(Pod(Point(6922,-3053), List(Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255)),
          Point(-0.249215005963018,-0.9684481817850932), Point(-17,-65), true),
          Some(Command(Point(2886.228847463513,1156.3448649671209),60,"PILOT0-CORRECTED"))),
          Record(Pod(Point(5192,-2487), List(Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255)),
          Point(-0.5105084131039128,-0.8598727581160626), Point(-29,-49), true),
          Some(Command(Point(10074.239426897095,-1135.9234506289276),60,"PILOT0-CORRECTED"))),
          Record(Pod(Point(4341,-2230), List(Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255)),
          Point(-0.3580156872661533,-0.933715571076837), Point(-24,-63), true),
          Some(Command(Point(10074.239426897095,-1135.9234506289276),60,"PILOT0-CORRECTED")))),
          List(Record(Pod(Point(6105,-2962), List(Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255)),
          Point(0.2298502103747638,-0.9732260173210932), Point(22,-164), true),
          Some(Command(Point(3731.003718664278,-5048.385737229311),125.76538187685465,"PILOT0-CORRECTED"))),
          Record(Pod(Point(6909,-3178), List(Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255)),
          Point(0.06224939096344839,-0.9980606260767326), Point(-12,-107),
          true),
          Some(Command(Point(2692.7863103516893,1496.4948207019925),60,"PILOT0-CORRECTED"))),
          Record(Pod(Point(5021,-2677), List(Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255)),
          Point(-0.711556089215331,-0.7026292990621612), Point(-146,-162),
          true),
          Some(Command(Point(10440.171798564217,-976.1539805655154),60,"PILOT0-CORRECTED"))),
          Record(Pod(Point(4279,-2340), List(Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255),
          Point(5655,-2567), Point(4093,-7460), Point(13488,-2344),
          Point(12948,-7255), Point(5655,-2567), Point(4093,-7460),
          Point(13488,-2344), Point(12948,-7255), Point(5655,-2567),
          Point(4093,-7460), Point(13488,-2344), Point(12948,-7255)),
          Point(-0.6290271316856146,-0.7773833466208085), Point(-53,-94), true),
          Some(Command(Point(10440.171798564217,-976.1539805655154),60,"PILOT0-CORRECTED"))))))
  }
}
