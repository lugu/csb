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
    val newSpeed = speed + desiredOrientation * command.thrust
    val angle = Angle.fromDegree(math.round(desiredOrientation.angleToEast.degree))
    val newOrientation = Point(1, 0).rotate(-angle)
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
  def newRecorder = RaceRecord(laps, checkpoints, List())

  def myPods = pods.take(2)
  def pod0 = pods(0)
  def pod1 = pods(1)

  def enemies = pods.drop(2)
  def enemy0 = pods(2)
  def enemy1 = pods(3)

  def friend(me: Pod) = if (me == pod0) pod1 else pod0

  def inverted: Race = Race(pods.drop(2) ::: pods.take(2), checkpoints, laps)

  def compareScore(a: Pod, b: Pod) = if (a.score < b.score) a
  else if (a.score > b.score) b
  else if (a.distance < b.distance) a else b

  def myLeader = compareScore(pod0, pod1)
  def enemyLeader = compareScore(enemy0, enemy1)

  def scoreMin = pods.map(_.score).min
  def isFirstTurn = if (scoreMin >= checkpoints.size * (laps - 1)) true else false
  def isLastTurn = if (scoreMin <= checkpoints.size) true else false

  def winner: Option[Pod] = pods.find(_.hasFinished)
  def isFinished = winner.isDefined

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
    val p2 = p.map(pod => {
      val collisions = for (other <- p; if (pod != other); if (pod.detectPossibleCollision(other, 0))) yield other
      val speedImpact = collisions.map(other => {
        val massCoef = 2
        val nx = pod.position.x - other.position.x
        val ny = pod.position.y - other.position.y
        val squareDist = nx*nx + ny*ny
        val dvx = pod.speed.x - other.speed.x
        val dvy = pod.speed.y - other.speed.y
        val product = nx*dvx + ny*dvy
        val fx = (nx*product) / (squareDist * massCoef)
        val fy = (ny*product) / (squareDist * massCoef)
        Point(- fx, - fy)
      }).foldLeft(Point(0, 0))(_ + _)
      Pod(pod.position + speedImpact, pod.destinations, pod.orientation, pod.speed, pod.boostAvailable)
    })
    Race(p2, checkpoints, laps)
  }
}

object Race {

  def parseInput(input: Stream[String]): Race = {
    def parsePods(checkpoints: List[Point], input: Stream[String]): List[Pod] = {
      val updater = PodUpdater(checkpoints)
      val destinations = (for (i ← 1 to checkpoints.size)
        yield checkpoints.tail :+ checkpoints.head).flatten.toList
      input.take(4).map{
        (line: String) => {
          val u = updater.parsePodUpdate(line)
          Pod(u.position, destinations, u.orientation, u.speed, true)
        }
      }.toList
    }
    val laps = input.head.toInt
    val checkpointsNb = input.tail.head.toInt
    val checkpoints = input.tail.tail.take(checkpointsNb).map{
      (line: String) => {
        val Array(checkpointX, checkpointY) = line.split(" ").map(_.toInt)
        Point(checkpointX, -checkpointY)
      }
    }.toList
    val pods = parsePods(checkpoints, input.drop(2 + checkpointsNb))
    Race(pods, checkpoints, laps)
  }

  def initPods(checkpoints: List[Point], laps: Int): List[Pod] = {
    val checkpointsShift = checkpoints.tail :+ checkpoints.head
    val destinations = (for (i ← 0 to laps) yield checkpointsShift).flatten.toList
    val departLine = (checkpoints(1) - checkpoints(0)).rotate(Degree(90)).normalize
    val positions = List(1, 3, -1, -3).map(pos ⇒ checkpoints(0) + departLine * (pos * 450))
    positions.map(p ⇒ Pod(p, destinations, (checkpoints(1) - p).normalize, Point(0, 0), true))
  }

}


case class PodUpdate(position: Point, destination: Point, orientation: Point, speed: Point) {
  def pod: Pod = Pod(position, List(destination), orientation, speed, false)
}

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

case class PodUpdater(checkpoints: List[Point]) {
  def parsePodUpdate(line: String): PodUpdate = {
      val Array(x, y, vx, vy, angle, index) = for (i ← line split " ") yield i.toInt
      // reverse Y coordinate as the input are non cartesian
      val position = Point(x, -y)
      val destination = checkpoints(index)
      val orientation = Point(1, 0).rotate(Degree(-angle))
      val speed = Point(vx, -vy)
      PodUpdate(position, destination, orientation, speed)
  }
}
