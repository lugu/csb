package csb

import math.{Pi}

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class PlayerSpec extends AnyFlatSpec with should.Matchers {

  def position = Point(0, 0)
  def speed = Point(100, 0)
  def orientation = Point(1, 0)
  def checkpoints = List(Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0))
  def pod = Pod(position, checkpoints, orientation, speed)

  "Pod" should "always be a Point" in {
    assert(pod.destination.isInstanceOf[Point])
  }

  it should "compute its angle to destination" in {
    assert(
      Pod(position, checkpoints, orientation, speed).angleToDest == Degree(0)
    )
    assert(
      Pod(
        position,
        checkpoints,
        orientation.rotate(Degree(90)),
        speed
      ).angleToDest == Degree(90)
    )
    assert(
      Pod(
        position,
        checkpoints,
        orientation.rotate(Degree(360 - 45)),
        speed
      ).angleToDest == Degree(-45)
    )
  }

  it should "compute its speed angle to destination" in {
    assert(
      Pod(position, checkpoints, orientation, speed).speedAngleToDest == Degree(
        0
      )
    )
    val angle1 = Pod(
      position,
      checkpoints,
      orientation,
      speed.rotate(Degree(90))
    ).speedAngleToDest.degree
    angle1 should equal(-90.0 +- 0.001)
    val angle2 = Pod(
      position,
      checkpoints,
      orientation,
      speed.rotate(Degree(360 - 45))
    ).speedAngleToDest.degree
    angle2 should equal(45.0 +- 0.001)
  }

  it should "have a radius of 400 while a checkpoint has a radius of 600" in {
    assert(pod.podRadius == 400)
    assert(pod.checkpointRadius == 600)
  }

  it should "compute accuratly its distance to another pod" in {
    val podA = Pod(Point(0, 0), checkpoints, orientation, speed)
    val podB = Pod(Point(1000, 0), checkpoints, orientation, speed)
    assert(podA.distanceToPod(podB) == (1000 - 2 * 400))
  }

  it should "compute its next position based on its current speed" in {
    val podA = Pod(position, checkpoints, orientation, Point(0, 0))
    val command = Move(Point(1, 0), 0, "test")
    val commandA = Move(Point(1, 0), 100, "test")
    assert(pod.update(command).position == (pod.position + pod.speed))
    assert(
      podA
        .update(commandA)
        .position == (pod.position + Point(1, 0).normalize * 100)
    )
    assert(
      pod
        .update(commandA)
        .position == (pod.position + pod.speed + Point(1, 0).normalize * 100)
    )
  }

  it should "always have boost available" in {
    pod.boostAvailable should be(true)
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

  it should "find closet point in a segment" in {
    assert(Point(0, 0).closest(Point(0, 0), Point(1, 1)) == Point(0, 0))
    assert(Point(0, 0).closest(Point(-1, -1), Point(1, 1)) == Point(0, 0))
    assert(Point(1, 1).closest(Point(0, 2), Point(2, 0)) == Point(1, 1))
    assert(Point(1, 1).closest(Point(0, 2), Point(2, 2)) == Point(1, 2))
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

  it should "compute the collision time" in {
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(0, 0)).collisionTime(
        Pod(Point(0, 0), checkpoints, orientation, Point(0, 0))
      ) == Some(0)
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(0, 0)).collisionTime(
        Pod(Point(400, 0), checkpoints, orientation, Point(0, 0))
      ) == Some(0)
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(0, 0)).collisionTime(
        Pod(Point(799, 0), checkpoints, orientation, Point(0, 0))
      ) == Some(0)
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(0, 0)).collisionTime(
        Pod(Point(800, 0), checkpoints, orientation, Point(0, 0))
      ) == None
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(200, 0)).collisionTime(
        Pod(Point(1000, 0), checkpoints, orientation, Point(0, 0))
      ) == Some(1)
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(400, 0)).collisionTime(
        Pod(Point(1000, 0), checkpoints, orientation, Point(0, 0))
      ) == Some(0.5)
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(400, 0)).collisionTime(
        Pod(Point(1000, 0), checkpoints, orientation, Point(400, 0))
      ) == None
    )
    assert(
      Pod(Point(0, 0), checkpoints, orientation, Point(400, 0)).collisionTime(
        Pod(Point(1000, 0), checkpoints, orientation, Point(100, 0))
      ) == Some(2.0 / 3)
    )
  }

  "Race" should "detect a winner and a looser" in {

    val podL = Pod(
      Point(0, 0),
      List(Point(0, 0)),
      Point(0, 0),
      Point(0, 0),
      100,
      true,
      true
    )
    val podW = Pod(Point(0, 0), List(), Point(0, 0), Point(0, 0))
    val raceN = Race(List(pod, pod, pod, pod), checkpoints, 1)
    val raceW = Race(List(pod, pod, pod, podW), checkpoints, 1)
    val raceL = Race(List(pod, podL, pod, pod), checkpoints, 1)

    assert(raceN.winner.isEmpty)
    assert(raceN.looser.isEmpty)
    assert(!raceN.winnerIsPlayerA)
    assert(!raceN.inverted.winnerIsPlayerA)

    assert(!raceW.winner.isEmpty)
    assert(raceW.looser.isEmpty)
    assert(!raceW.winnerIsPlayerA)
    assert(raceW.inverted.winnerIsPlayerA)

    assert(raceL.winner.isEmpty)
    assert(!raceL.looser.isEmpty)
    assert(!raceL.winnerIsPlayerA)
    assert(raceL.inverted.winnerIsPlayerA)
  }

  "Race" should "be able to manage its checkpoints" in {

    val checkpoints = List(
      Point(0, 0),
      Point(1, 1) * 1000,
      Point(2, 2) * 1000,
      Point(3, 3) * 1000
    )
    val race = Race(List(pod, pod, pod, pod), checkpoints, 1)

    assert(!race.checkpoints.isEmpty)
    assert(race.nextCheckpoint(checkpoints(0)) == checkpoints(1))
    assert(race.nextCheckpoint(checkpoints(3)) == checkpoints(0))
    assert(race.checkpointIndex(checkpoints(3)) == 3)
    assert(race.checkpointIndex(checkpoints(0)) == 0)
  }

  "RaceRecord" should "generate a Race at any step" in {
    val checkpoints = List(
      Point(0, 0),
      Point(1, 1) * 1000,
      Point(2, 2) * 1000,
      Point(3, 3) * 1000
    )
    val pods = Pod(Point(0, 0), List(Point(0, 0)), Point(0, 0), Point(0, 0))
    val recorded = RaceRecord(1, checkpoints, List())
  }
}
