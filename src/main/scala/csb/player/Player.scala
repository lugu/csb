package csb.player

import math._
import math.{ sqrt, pow, cos, sin, atan, Pi }
import scala.util._

case class Angle(radian: Double) {
  // internal representation in radian from -Pi to Pi.
  def +(other: Angle) = Radian(radian + other.radian)
  def -(other: Angle) = Radian(radian - other.radian)
  def *(scale: Double) = Radian(radian * scale)
  // compare absolute angle
  def <(other: Angle): Boolean =
    if (radian * radian < other.radian * other.radian) true
    else false
  // compare absolute angle
  def >(other: Angle): Boolean =
    if (radian * radian > other.radian * other.radian) true
    else false
  def unary_- = Radian(-radian)
  def degree: Double = radian / Pi * 180
  override def toString = degree.toString + "°"
}

object Degree {
  def apply(degree: Double) = Angle.fromDegree(degree)
}

object Radian {
  def apply(radian: Double) = Angle.fromRadian(radian)
}

object Angle {
  def fromDegree(angle: Double) = Radian(angle / 180 * Pi)
  def fromRadian(radian: Double): Angle = {
    val r = radian % (2 * Pi)
    if (r <= -Pi) new Angle(r + 2 * Pi)
    else if (r > Pi) new Angle(r - 2 * Pi)
    else new Angle(r)
  }
}

case class Point(x: Double, y: Double) {
  override def toString = s"($x,$y)"
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
  def unary_- = Point(-x, -y)
  def *(factor: Double) = Point(x * factor, y * factor)
  def distanceTo(other: Point): Double =
    sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))
  def toInt = Array(x.toInt, y.toInt)
  def normalize: Point = if (this == Point(0, 0)) this
  else Point(x / norm, y / norm)
  def scalar(other: Point): Double = x * other.x + y * other.y
  def squareNorm = x * x + y * y
  lazy val norm = sqrt(squareNorm)
  def radianWith(other: Point) =
    Radian(atan2(y, x) - atan2(other.y, other.x))
  def radianFrom(other: Point) =
    Radian(atan2(other.y, other.x) - atan2(y, x))
  def rotate(angle: Angle): Point = {
    val ca = cos(angle.radian)
    val sa = sin(angle.radian)
    Point(ca * x - sa * y, sa * x + ca * y)
  }
  def round = Point(x.round, y.round)
  def floor = Point(x.floor, y.floor)
}

object Point {
  def apply(a: Int, b: Int): Point = Point(a.toDouble, b.toDouble)

}

trait Pilot {
  def direction: Point
  def thrust: Double
  def label: String
  def command = Command(direction, thrust, label)
}

case class PilotTest(pod: Pod) extends Pilot {
  def label = "TEST"
  def direction = pod.position + (pod.orientation * 1000) + Point(-100, 0)
  def thrust = 100
  // Print(">>> Current pod: " + pod)
  // Print(">>> Next pod: " + pod.update(direction, thrust))
}

case class PilotCorrected(pilot: Pilot, pod: Pod, race: Race) extends Pilot {
  def label = pilot.label + "-CORRECTED"
  // correction of the direction takes inertia into account
  def direction = pod.position +
    ((pilot.direction - pod.position).normalize - pod.speed.normalize * 0.5) * 5000

  def thrust = {
    if (pod.badCollision(race.enemy0))
      Pilot.shield
    else if (pod.badCollision(race.enemy1))
      Pilot.shield
    else
      min(thrustCorrection, pilot.thrust)
  }

  def thrustCorrection = {
    val maxAngle = Degree(18)
    val minAngle = Degree(90)
    val minSpeed = 60

    val directionToDestinationAngle =
      (pod.destination - pod.position).radianWith(pilot.direction - pod.position)
    val directionToOrientationAngle = pod.angleToDest + directionToDestinationAngle

    // Print("directionToDestinationAngle ", directionToDestinationAngle)
    // Print("directionToOrientationAngle ", directionToOrientationAngle)

    if (pod.badCollision(race.enemy0))
      Pilot.shield
    else if (pod.badCollision(race.enemy1))
      Pilot.shield
    else if (directionToOrientationAngle < maxAngle) 200
    else if (directionToOrientationAngle > minAngle) minSpeed
    else {
      val ratio = 1 - (abs(directionToOrientationAngle.degree) - maxAngle.degree) /
        (minAngle.degree - maxAngle.degree)
      minSpeed + (150 - minSpeed) * ratio
    }
  }
}

object Pilot {
  def boost = -1
  def shield = -2
  def thrustMax = -3
  def apply(pod: Pod, race: Race): Pilot = MetaPilot(pod, race)
}

case class MetaPilot(pod: Pod, race: Race) extends Pilot {

  def direction = pilot.direction
  def thrust = pilot.thrust
  def label = pilot.label

  val friend = race.friend(pod)

  val pilot: Pilot = {
    PilotCorrected(chooseAvoid
      .getOrElse(chooseFight
        .getOrElse(chooseDefense
          .getOrElse(chooseBoost
            .getOrElse(chooseSkip
              .getOrElse(choosePilot1
                .getOrElse(Pilot0(pod))))))), pod, race)
  }

  val init = {
    // Print(this)
  }

  override def toString = pod.toString
  // override def toString = pod.toString +
  //     "\ndirection to destination angle: " +
  //     (direction - pod.position).radianFrom(pod.destinationDirection) +
  //     "\ndirection to position angle: " +
  //     (direction - pod.position).radianFrom(pod.destinationDirection)

  def chooseDefense: Option[Pilot] = {
    if (fightCondition) None
    // else if (pod.badCollision(race.friend(pod)))
    //     Some(PilotDefense(pod))
    else if (pod.badCollision(race.enemy0))
      Some(PilotDefense(pod))
    else if (pod.badCollision(race.enemy1))
      Some(PilotDefense(pod))
    else None
  }

  def chooseAttack: Option[Pilot] = {
    val someSpeed = 200
    val e = race.enemies.filter(o ⇒ pod.detectCollision(o))
    if (!e.isEmpty && pod.score > friend.score)
      Some(PilotAttack(pod, e.head))
    else None
  }

  def fightCondition: Boolean = {
    // Print("Last turn? ", race.isLastTurn)
    // if (!pod.isLeader(race) && race.isLastTurn &&
    //     race.compareScore(race.enemyLeader, race.myLeader) == race.enemyLeader)
    //         true else false
    // FIXME:
    false
  }

  def chooseNone: Option[Pilot] = None

  def chooseFight: Option[Pilot] =
    if (fightCondition) {
      Some(PilotFight(pod, race, race.enemyLeader))
    }
    else None

  def chooseAvoid: Option[Pilot] = {
    if (!race.isFirstTurn && pod.detectCollision(friend) &&
      race.compareScore(pod, race.friend(pod)) == friend)
      Some(PilotAvoid(pod, friend))
    else None
  }

  // choose boost for the longest distance
  def chooseBoost: Option[Pilot] = {
    val smallAngle = Degree(30)
    val longDistance = 4000

    if (pod.boostCollide(race.friend(pod)) ||
      pod.boostCollide(race.enemy0) ||
      pod.boostCollide(race.enemy1)) None
    else if (pod.boostAvailable &&
      pod.destination == race.boostCheckpoint &&
      pod.angleToDest < smallAngle &&
      pod.distance > longDistance) {
      Some(PilotBoost(pod))
    }
    else None
  }

  // do not choose skip if a collision is detected
  def chooseSkip: Option[Pilot] = {
    val skipAngle = Degree(30)
    val skipDistance = 2000
    val skipSpeed = 350
    if (pod.detectCollision(race.friend(pod))
      || pod.detectCollision(race.enemy0)
      || pod.detectCollision(race.enemy1)) None
    else if (pod.stepsToDestination < 2 &&
      pod.checkSpeedCheckpoint(pod.destination) &&
      pod.speed.norm > skipSpeed) Some(PilotSkip(pod, race))
    else None
  }

  def choosePilot1: Option[Pilot] = {
    val directDistance = 5000
    if (pod.distance > directDistance) Some(Pilot1(pod, race))
    else None
  }
}

case class PilotHit(pod: Pod, race: Race, enemy: Pod) extends Pilot {
  def label = "HIT"
  def thrust = if (pod.detectCollision(enemy)) {
    if (pod.boostAvailable) {
      Pilot.boost
    }
    else Pilot.shield
  }
  else 200
  // case 1: i am in front of the enemy
  // case 2: i am behind the enemy

  def direction1 = enemy.position + (enemy.speed * 2)

  // direction is the point in the middle of the next checkpoint
  // compute the distance between the enemy and his checkpoint
  // add this distance the
  def direction2 = enemy.destination +
    (enemy.nextDestination(race) - enemy.destination).normalize * enemy.distance

  def direction = if (pod.position.distanceTo(enemy.destination) < enemy.distance)
    direction1 else direction2
}

case class PilotWait(pod: Pod, race: Race, checkpoint: Point) extends Pilot {
  def previousCheckpoint = race.previousCheckpoint(checkpoint)
  def directionToPrevious = (previousCheckpoint - checkpoint).normalize
  val position = checkpoint + (directionToPrevious * (pod.checkpointRadius * 2))

  val dist = max(pod.position.distanceTo(position) - pod.podRadius, 0)
  val smallDistance = 2500

  def label = "WAIT"
  def direction = position
  def thrust = {
    val t = if (dist > smallDistance) 200 else (dist / smallDistance) * 200
    Print("dis " + dist)
    Print("thrust " + t)
    t
  }
}

case class PilotAvoid(pod: Pod, other: Pod) extends Pilot {
  def label = "AVOID"

  def distanceBeforeMove = pod.position.distanceTo(other.position + other.speed)
  def distanceAfterMove = (pod.position + pod.speed).distanceTo(other.position + other.speed)

  // goal: direction opposite to the other
  def direction = pod.position + (other.position - pod.position).rotate(Degree(90))

  def thrust = if (distanceBeforeMove > distanceAfterMove) 0 else 200
}

case class PilotFight(pod: Pod, race: Race, enemy: Pod) extends Pilot {
  val veryLarge = 3000
  val pilot = if (pod.distanceToPod(enemy) < veryLarge ||
    enemy.score > race.laps * race.checkpoints.size - 2)
    PilotHit(pod, race, enemy)
  else if (pod.position.distanceTo(enemy.destination) <
    pod.position.distanceTo(enemy.nextDestination(race)))
    PilotWait(pod, race, enemy.destination)
  else
    PilotWait(pod, race, enemy.nextDestination(race))

  def direction = pilot.direction
  def thrust = pilot.thrust
  def label = pilot.label
}

case class PilotDefense(pod: Pod) extends Pilot {
  def direction = pod.destination
  def thrust = Pilot.shield
  def label = "DEFENSE"
}

case class PilotAttack(pod: Pod, enemy: Pod) extends Pilot {
  def direction = enemy.position + enemy.speed
  def thrust = Pilot.shield
  def label = "ATTACK"
}

case class PilotBoost(pod: Pod) extends Pilot {
  def direction = pod.destination
  def thrust = Pilot.boost
  def label = "BOOST"
}

case class PilotSkip(pod: Pod, race: Race) extends Pilot {
  def label = "SKIP"
  def direction = pod.nextDestination(race)
  def thrust = Pilot.thrustMax
}

// if close enouth from the ckeckpoint, set destination to the vector between the
// checkpoint and the next one
// if far from the checkpoint, set goal to a interior of the turn
// move your previous destination in the direction of the goal
// if turn angle > some value then stop the motors
case class Pilot3(pod: Pod, race: Race) extends Pilot {

  val skidAngle = Degree(30)
  val skidSpeed = 250

  val shallSkid: Boolean = if (pod.stepsToDestination < 4 &&
    pod.speedAngleToDest < skidAngle &&
    pod.speed.norm > skidSpeed) true else false

  def skidDirection = pod.position + (pod.nextDestination(race) - pod.destination).normalize

  def direction = if (shallSkid) skidDirection else pod.innerDirection(race)

  def thrust = 200
  def label = "PILOT3"
}

case class Pilot1(pod: Pod, race: Race) extends Pilot {

  def newAngle0(angle: Angle) = {
    val ratio = 2
    // angle in [-Pi, Pi]
    if (angle < Radian(0)) -Radian((-Pi / ratio) - (angle.radian / ratio))
    else -Radian((Pi / ratio) - (angle.radian / ratio))
  }

  def newAngle1(angle: Angle) = Radian(sin(angle.radian) / 4)

  def direction = {
    val a = -pod.destinationDirection
    val b = (pod.nextDestination(race) - pod.destination).normalize
    val angle = a.radianFrom(b)
    val newAngle: Angle = newAngle1(angle)
    val objective = (pod.destination - pod.position).rotate(newAngle)
    pod.position + objective
  }

  def thrust = 200
  def label = "PILOT1"
}

case class Pilot0(pod: Pod) extends Pilot {
  def direction = pod.destination
  def thrust = 200
  def label = "PILOT0"
}

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

  override def toString = "position: " + position +
    "\ndestination: " + destination +
    "\norientation: " + orientation +
    "\nangleToDest: " + angleToDest +
    "\nspeed: " + speed +
    "\nspeed norm: " + speed.norm

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

      Print("speed " + speed)
      Print("otherDirection " + otherDirection)
      Print("collisionAngleToSpeed " + collisionAngleToSpeed)
      Print("speedAngle " + speedAngle)

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
    val newOrientation = orientation.rotate(Degree(max(-18, min(18, orientation.radianWith(expectedOrientation).degree))))
    val newSpeed = speed + newOrientation * command.thrust
    Pod((position + newSpeed).round, destinations, newOrientation, (newSpeed * 0.85).floor, boostAvailable).updateDestination
  }

  def updateWith(u: PodUpdate): Pod = {
    val dests = if (u.destination == destination) destinations.tail else destinations
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

case class Race(
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
      Print("can not find next checkpoint to ", p)
      checkpoints.zipWithIndex.foreach(Print(_))
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
      Print("can not find next checkpoint to ", p)
      checkpoints.zipWithIndex.foreach(Print(_))
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
    val destinations = (for (i ← 0 to laps) yield checkpoints).flatten.toList
    val departLine = (checkpoints(1) - checkpoints(0)).rotate(Degree(90)).normalize
    val positions = List(1, 3, -1, -3).map(pos ⇒ checkpoints(0) + departLine * (pos * 450))
    positions.map(p ⇒ Pod(p, destinations, (checkpoints(1) - p).normalize, Point(0, 0), true))
  }

}

case class Command(direction: Point, thrust: Double, label: String) {
  def answer: String = {
    val t = thrust.round.toInt
    val s = if (t == Pilot.boost) "BOOST"
    else if (t == Pilot.shield) "SHIELD"
    else if (t == Pilot.thrustMax) "200"
    else t.toString
    val x: Int = direction.x.round.toInt
    val y: Int = direction.y.round.toInt
    // reverse Y coordinate as the input are non cartesian
    "" + x + " " + (-y) + " " + s + " " + s + " " + label
  }
}

object Print {
  var printer: (String) ⇒ Unit = Console.err.println(_)
  def setPrinter(p: String ⇒ Unit) = {
    printer = p
  }
  def apply(messages: Object*) = {
    printer(messages.mkString(" "))
  }
}

case class Player(var race: Race) {
  var pilots: List[Pilot] = List(Pilot(race.pod0, race), Pilot(race.pod1, race))
  def commands = pilots.map(_.command)
}

case class PodUpdate(position: Point, destination: Point, orientation: Point, speed: Point) 

case class Record(pod: Pod, command: Option[Command])
case class RaceRecord(laps: Int, checkpoints: List[Point], steps: List[Array[Record]]) {
    def updateWith(record: Array[Record]): RaceRecord = {
      RaceRecord(laps, checkpoints, steps ::: List(record))
    }
    def dump(): Unit = {
      Print(laps.toString)
      Print(checkpoints)
      steps.foreach(Print(_))
    }
    def step(i: Int): Race = Race(steps(i).map(_.pod).toList, checkpoints, laps)
}

object PodUpdate {
  def apply(checkpoints: List[Point]): PodUpdate = {
      val Array(x, y, vx, vy, angle, index) = for (i ← readLine split " ") yield i.toInt
      // reverse Y coordinate as the input are non cartesian
      val position = Point(x, -y)
      val destination = checkpoints(index)
      val orientation = Point(1, 0).rotate(Degree(-angle))
      val speed = Point(vx, -vy)
      PodUpdate(position, destination, orientation, speed)
  }
}

object Player extends App {

  val Array(laps) = for (i ← readLine split " ") yield i.toInt
  val Array(checkpointNb) = for (i ← readLine split " ") yield i.toInt
  val checkpoints: List[Point] = (for (i ← 1 to checkpointNb) yield {
    val Array(checkpointX, checkpointY) = for (i ← readLine split " ") yield i.toInt
    // reverse Y coordinate as the input are non cartesian
    Point(checkpointX, -checkpointY)
  }).toList

  var pods: List[Pod] = (for (i ← 1 to 4) yield {
    val u = PodUpdate(checkpoints)
    Pod(u.position, checkpoints, u.orientation, u.speed, true)
  }).toList

  var race = Race(pods, checkpoints, laps)

  var recorder = RaceRecord(laps, checkpoints, List())

  while (!race.isFinished) {
    val player = Player(race)
    val commands = player.commands
    val record = Array(Record(pods(0), Some(commands(0))), Record(pods(1), Some(commands(1))), 
                      Record(pods(2), None), Record(pods(3), None))
    recorder = recorder.updateWith(record)

    if (race.isFinished) {
      recorder.dump()
    } else {
      commands.foreach(c => println(c.answer))
      pods = for (p ← pods) yield { p.updateWith(PodUpdate(checkpoints)) }
      race = Race(pods, checkpoints, laps)
    }
  }
}
