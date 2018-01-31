package csb

import math._
import math.{ sqrt, Pi }
import scala.util._

case class Pod(
    val position:       Point,
    val destinations:   List[Point],
    val orientation:    Point,
    val speed:          Point,
    val boostAvailable: Boolean,
    val hasShield: Boolean = false) {

  def this(p: Point, d: List[Point], o: Point, s: Point) = this(p, d, o, s, true)

  lazy val destination = if (destinations.isEmpty) position else destinations.head
  def angleToDest = orientation.radianWith(destination - position)

  def speedAngleToDest = speed.radianFrom(destinationDirection)

  override def toString = s"Pod($position, List($destination), $orientation, $speed, $boostAvailable)"
  def data = position.data ++ destination.data ++ orientation.data ++ speed.data ++ Array(0.0)

  def checkpointRadius = 600
  def podRadius = 400

  def stepsToDestination = ((distance - checkpointRadius) / speed.norm).toInt
  def nextPosition = position + speed

  def distanceToPod(other: Pod) = (other.position - position).norm - 2 * podRadius
  def collisionTime(other: Pod): Option[Double] = {
    val squareDist = (position).squareDistanceTo(other.position)
    val dist = (position).distanceTo(other.position)
    val squareRadii = (podRadius + other.podRadius) * (podRadius + other.podRadius)
    if (squareDist < squareRadii) return Some(0)
    val x = position.x - other.position.x
    val y = position.y - other.position.y
    val myp = Point(x, y)
    val vx = speed.x - other.speed.x
    val vy = speed.y - other.speed.y
    val up = Point(0, 0)
    val p = up.closest(myp, Point(x + vx, y + vy))

    // Square of the distance between u and the closest point to u on
    // the line described by our speed vector
    val pdist = up.squareDistanceTo(p)

    // Square of the distance between us and that point
    val mypdist = myp.squareDistanceTo(p)

    // If the distance between u and this line is less than the sum of
    // the radii, there might be a collision
    if (pdist < squareRadii) {
      // Our speed on the line
        val length = sqrt(vx*vx + vy*vy)
        if (length == 0) return None // immobile object won't collide

        // We move along the line to find the point of impact
        val backdist = sqrt(squareRadii - pdist)
        val p2 = p - Point(backdist * (vx / length), backdist * (vy / length))

        // If the point is now further away it means we are not going
        // the right way, therefore the collision won't happen
        if (myp.squareDistanceTo(p2) > mypdist) {
            return None
        }

        val pdist2 = p2.distanceTo(myp);

        // The point of impact is further than what we can travel in
        // one turn
        if (pdist2 > length) {
            return None
        }

        // Time needed to reach the impact point
        val t = pdist2 / length;

        return Some(t)
    }
    return None
  }
  def bounce(other: Pod): Pod = {
    // If a pod has its shield active its mass is 10 otherwise it's 1
    val m1 = if (hasShield) 10 else 1
    val m2 = if (other.hasShield) 10 else 1
    val mcoeff = (m1 + m2) / (m1 * m2)

    val n = position - other.position

    // Square of the distance between the 2 pods. This value could be hardcoded because it is always 800²
    val nxnysquare = position.squareDistanceTo(other.position)

    val dv = speed - other.speed

    // fx and fy are the components of the impact vector. product is just there for optimisation purposes
    val product = n.x*dv.x + n.y*dv.y
    val f = n * (product / nxnysquare * mcoeff)

    // We apply the impact vector once
    val newSpeed = speed - (f * (1.0 / m1))
    val otherNewSpeed = other.speed + (f * (1.0 / m2))

    // If the norm of the impact vector is less than 120, we normalize it to 120
    val impulse = sqrt(f.x*f.x + f.y*f.y)
    val newF = if (f.norm < 120.0) f.normalize * 120.0 else f

    // We apply the impact vector a second time
    val newNewSpeed = newSpeed - (f * (1.0 / m1))
    val newOtherNewSpeed = otherNewSpeed + (f * ( 1.0 / m2))

    // This is one of the rare places where a Vector class would have made the code more readable.
    // But this place is called so often that I can't pay a performance price to make it more readable.
    // Pod(position, destinations, orientation, newNewSpeed, boostAvailable, hasShield)
    Pod(position, destinations, orientation, newSpeed, boostAvailable, hasShield)
  }

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
    val thrust = if (command.thrust == Pilot.boost) {
      if (!boostAvailable) {
        Print(s"WARNING: BOOST is not available to $this ($command)" )
        0
      } else 650
    } else if (command.thrust == Pilot.shield) 0 else command.thrust
    val hasBoost = if (command.thrust == Pilot.boost) false else boostAvailable
    val hasShiled = if (command.thrust == Pilot.shield) true else false
    val expectedOrientation = (command.direction - position).normalize
    val desiredOrientation = orientation.rotate(Degree(max(-18, min(18, expectedOrientation.radianWith(orientation).degree))))
    val newSpeed = speed + desiredOrientation * thrust
    val angle = Angle.fromDegree(math.round(desiredOrientation.angleToEast.degree))
    val newOrientation = Point(1, 0).rotate(-angle)
    Pod((position + newSpeed).round, destinations, newOrientation, (newSpeed * 0.85).floor, hasBoost, hasShield).updateDestination
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

  def simulate(commands: List[Command]): Race =
    simulateCommands(commands).simulateCollision

  def simulateCommands(commands: List[Command]): Race = {
    val p = pods.zip(commands).map {
      case (pod: Pod, c: Command) ⇒
        pod.update(c)
    }
    Race(p, checkpoints, laps)
  }

  def simulateCollision: Race = {
    val p = pods.map(pod => {
      val collisions: List[Tuple2[Pod,Double]] = pods.filter(p => p != pod).flatMap{ other =>
        val collision: Option[Tuple2[Pod,Double]] = pod.collisionTime(other).map(t => (other, t))
        collision
      }
      collisions.sortBy{ case (p: Pod, t: Double) => t }.map{
        case (p: Pod, t: Double) => p
        }.foldLeft(pod){
          case (p: Pod, other: Pod) => p.bounce(other)
        }
    })
    Race(p, checkpoints, laps)
  }
}

object Race {


  def random: Race = {
    import scala.util.Random

    val width = 16000
    val margin = 400
    val height = 9000
    def randomWidth = margin + Random.nextInt(width - 2 * margin)
    def randomHeight = margin + Random.nextInt(height - 2 * margin)
    def randomPosition = Point(randomWidth, - randomHeight)
    val laps = Random.nextInt(6)
    val checkpointsNb = 3 + Random.nextInt(2)
    val checkpoints = (1 to checkpointsNb).map(i => randomPosition)
    Race(checkpoints.toList, laps)
  }

  def parseInput(input: Stream[String]): Race = {
    def parsePods(checkpoints: List[Point], input: Stream[String]): List[Pod] = {
      val updater = PodUpdater(checkpoints)
      val destinations = (for (i ← 1 to checkpoints.size)
        yield checkpoints.tail :+ checkpoints.head).flatten.toList
      input.take(4).map{
        (line: String) => {
          val u = updater.parsePodUpdate(line)
          Pod(u.position, destinations, (destinations.head - u.position).normalize, u.speed, true)
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

  def apply(checkpoints: List[Point], laps: Int): Race = {
    val checkpointsShift = checkpoints.tail :+ checkpoints.head
    val destinations = (for (i ← 0 to laps) yield checkpointsShift).flatten.toList
    val departLine = (checkpoints(1) - checkpoints(0)).rotate(Degree(90)).normalize
    val positions = List(1, 3, -1, -3).map(pos ⇒ checkpoints(0) + departLine * (pos * 450))
    val pods = positions.map(p ⇒ Pod(p, destinations, (checkpoints(1) - p).normalize, Point(0, 0), true))
    Race(pods, checkpoints, laps)
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
