package csb

import math._
import math.{ sqrt, Pi }
import scala.util._

case class Pod(
    val position:       Point,
    val destinations:   List[Point],
    val orientation:    Point,
    val speed:          Point,
    val boostAvailable: Boolean) {

  def this(p: Point, d: List[Point], o: Point, s: Point) = this(p, d, o, s, true)

  lazy val destination = if (destinations.isEmpty) position else destinations.head
  def angleToDest = orientation.radianWith(destination - position)

  def speedAngleToDest = speed.radianFrom(destinationDirection)

  override def toString = s"Pod($position, List($destination), $orientation, $speed, $boostAvailable)"
  def data = position.data ++ destination.data ++ orientation.data ++ speed.data ++ Array(0.0)

  val checkpointRadius = 600
  val podRadius = 400

  def stepsToDestination = ((distance - checkpointRadius) / speed.norm).toInt
  def nextPosition = position + speed

  def distanceToPod(other: Pod) = (other.position - position).norm - 2 * podRadius
  def detectCollision(other: Pod) = detectPossibleCollision(other, 200)
  def detectPossibleCollision(other: Pod, extra: Int) = {
    val dist = (position + speed).distanceTo(other.position + other.speed)
    if (dist < podRadius * 2 + extra) true else false
  }

  // bad collision is a collision that oppose the pod speed
  // 1. is the other in front of the speed
  def badCollision(other: Pod) = {
    if (detectCollision(other)) {

      val otherDirection = ((other.position + other.speed) - (position + speed))
      val collisionAngleToSpeed = speed.radianWith(otherDirection)
      val speedAngle = speed.radianWith(other.speed)

      // Print("speed " + speed)
      // Print("otherDirection " + otherDirection)
      // Print("collisionAngleToSpeed " + collisionAngleToSpeed)
      // Print("speedAngle " + speedAngle)

      if (collisionAngleToSpeed < Degree(45) && speedAngle > Degree(45)) true
      else false
    }
    else false
  }

  lazy val distance = position.distanceTo(destination)

  def nextDestination(race: Race): Point = race.nextCheckpoint(destination)

  lazy val destinationDirection = (destination - position).normalize

  def checkSpeedCheckpoint(checkpoint: Point): Boolean =
    Pod.distanceToLine(position, speed, checkpoint) < checkpointRadius

  def innerDirection(race: Race) = {
    val a = position - destination
    val b = nextDestination(race) - destination
    def turnAngle = a.radianFrom(b)
    def innerVector = a.rotate(turnAngle * 0.25)
    destination + innerVector * (3 * speed.norm)
  }

  def isLeader(race: Race): Boolean = (race.myLeader == this)

  def boostCollide(other: Pod): Boolean = {
    val p = position
    val u = speed
    val t = other.position
    if (Pod.distanceToLine(p, u, t) < podRadius) true else false
  }

  def hasReachDestination: Boolean = !hasFinished && ((position - destination).norm <= checkpointRadius)
  def hasFinished: Boolean = destinations.isEmpty
  def score = destinations.length

  def updateDestination: Pod = if (hasReachDestination)
    Pod(position, destinations.tail, orientation, speed, boostAvailable) else this

  def update(command: Command) = {
    val expectedOrientation = (command.direction - position).normalize
    val desiredOrientation = orientation.rotate(Degree(max(-18, min(18, expectedOrientation.radianWith(orientation).degree))))
    val angle = desiredOrientation.radianWith(Point(1, 0))
    val newOrientation = Point(1, 0).rotate(angle)
    val newSpeed = speed + newOrientation * command.thrust
    Pod((position + newSpeed).round, destinations, newOrientation, (newSpeed * 0.85).floor, boostAvailable).updateDestination
  }

  def updateWith(u: PodUpdate): Pod = {
    val dests = if (u.destination == destination) destinations else destinations.tail
    Pod(u.position, dests, u.orientation, u.speed, boostAvailable)
  }
}

object Pod {

  def distanceToLine(linePoint: Point, lineDirection: Point, testPoint: Point): Double =
    List(linePoint, lineDirection, testPoint) match {
      case List(Point(x, y), Point(u, v), Point(a, b)) ⇒
        abs(v * a - u * b - v * x + u * y) / sqrt(v * v + u * u)
    }
}

case class Race (
    val pods:        List[Pod],
    val checkpoints: List[Point],
    val laps:        Int) {

  def this(checkpoints: List[Point], laps: Int) = this(Race.initPods(checkpoints, 3), checkpoints, 3)

  def pod0 = pods(0)
  def pod1 = pods(1)

  def enemies = pods.slice(2, 4)
  def enemy0 = pods(2)
  def enemy1 = pods(3)

  def friend(me: Pod) = if (me == pod0) pod1 else pod0

  def inverted: Race = Race(pods.slice(2, 4) ::: pods.slice(0, 2), checkpoints, laps)

  def compareScore(a: Pod, b: Pod) = if (a.score < b.score) a
  else if (a.score > b.score) b
  else if (a.distance < b.distance) a else b

  def myLeader = compareScore(pod0, pod1)
  def enemyLeader = compareScore(enemy0, enemy1)

  def scoreMin = pods.map(_.score).min
  def isFirstTurn = if (scoreMin >= checkpoints.size * (laps - 1)) true else false
  def isLastTurn = if (scoreMin <= checkpoints.size) true else false
  def isFinished = pods.exists(_.hasFinished)

  def checkpointIndex(p: Point) = checkpoints.zipWithIndex.filter {
    case (target, index) ⇒ target == p
  }.head._2

  lazy val boostCheckpoint: Point = {
    val loop = checkpoints ::: List(checkpoints.head)
    val maxDist = loop.sliding(2).toList.map {
      case List(a, b) ⇒ a.distanceTo(b)
    }.max
    loop.sliding(2).filter {
      case List(a, b) ⇒ a.distanceTo(b) == maxDist
    }.toList.head(1)
  }

  def previousCheckpoint(p: Point): Point = {
    val loop = checkpoints ::: List(checkpoints.head)
    val found: List[Point] = loop.sliding(2).toList.filter {
      case List(a, b) ⇒ b == p
    }.map {
      case List(a, b) ⇒ a
    }
    if (!found.isEmpty) {
      val target = found.head
      target
    }
    else {
      // Print("can not find next checkpoint to ", p)
      // checkpoints.zipWithIndex.foreach(Print(_))
      Point(0, 0)
    }
  }
  def nextCheckpoint(p: Point): Point = {
    val loop = checkpoints ::: List(checkpoints.head)
    val found: List[Point] = loop.sliding(2).toList.filter {
      case List(a, b) ⇒ a == p
    }.map {
      case List(a, b) ⇒ b
    }
    if (!found.isEmpty) {
      val target = found.head
      target
    }
    else {
      // Print("can not find next checkpoint to ", p)
      // checkpoints.zipWithIndex.foreach(Print(_))
      Point(0, 0)
    }
  }

  def simulate(commands: List[Command]): Race = {
    val p = pods.zip(commands).map {
      case (pod: Pod, c: Command) ⇒
        pod.update(c)
    }
    Race(p, checkpoints, laps)
  }
}

object Race {

  def initPods(checkpoints: List[Point], laps: Int): List[Pod] = {
    val checkpointsShift = checkpoints.tail :+ checkpoints.head
    val destinations = (for (i ← 0 to laps) yield checkpointsShift).flatten.toList
    val departLine = (checkpoints(1) - checkpoints(0)).rotate(Degree(90)).normalize
    val positions = List(1, 3, -1, -3).map(pos ⇒ checkpoints(0) + departLine * (pos * 450))
    positions.map(p ⇒ Pod(p, destinations, (checkpoints(1) - p).normalize, Point(0, 0), true))
  }

}


case class PodUpdate(position: Point, destination: Point, orientation: Point, speed: Point) 

case class Record(pod: Pod, command: Option[Command]) {
  override def toString = s"Record($pod, $command)"

  def data: Array[Double] = command match {
      case None => pod.data ++ Array(0.0, 0.0, 0.0)
      case Some(c) => pod.data ++ c.data
  }
}

case class RaceRecord(laps: Int, checkpoints: List[Point], steps: List[List[Record]]) {
    def updateWith(race: Race, commands: List[Option[Command]]) = {
      val record = List(Record(race.pods(0), commands(0)), Record(race.pods(1), commands(1)), 
                      Record(race.pods(2), commands(3)), Record(race.pods(3), commands(3)))
      RaceRecord(laps, checkpoints, steps ::: List(record))
    }
    def dump(): Unit = Print(toString)
    override def toString = s"RaceRecord($laps, $checkpoints, $steps)"

    def data: Array[Double] = {
      val a: Array[Double] = checkpoints.map(_.data).flatten.toArray ++ steps.flatten.map(_.data).flatten
      Array(laps.toDouble, checkpoints.size) ++ a
    }

    def step(i: Int): Race = Race(steps(i).map(_.pod).toList, checkpoints, laps)
    def stepCommands(i: Int): List[Option[Command]] = steps(i).map(_.command).toList
}

object PodUpdate {
  def apply(checkpoints: List[Point]): PodUpdate = {
      val Array(x, y, vx, vy, angle, index) = for (i ← Input() split " ") yield i.toInt
      // reverse Y coordinate as the input are non cartesian
      val position = Point(x, -y)
      val destination = checkpoints(index)
      val orientation = Point(1, 0).rotate(Degree(-angle))
      val speed = Point(vx, -vy)
      PodUpdate(position, destination, orientation, speed)
  }
}
