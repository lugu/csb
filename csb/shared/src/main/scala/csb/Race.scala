package csb

import math._
import math.{sqrt, Pi}
import scala.util._

case class Pod(
    val position: Point,
    val destinations: List[Point],
    val orientation: Point,
    val speed: Point,
    val steps: Int,
    val boostAvailable: Boolean,
    val hasShield: Boolean
) {

  lazy val destination =
    if (destinations.isEmpty) position else destinations.head
  def angleToDest = orientation.radianWith(destination - position)

  def speedAngleToDest = speed.radianFrom(destinationDirection)

  override def toString =
    s"Pod($position, List($destination), $orientation, $speed, $steps, $boostAvailable, $hasShield)"
  def data =
    position.data ++ destination.data ++ orientation.data ++ speed.data ++ Array(
      0.0
    )

  def checkpointRadius = 600
  def podRadius = 400

  def stepsToDestination = ((distance - checkpointRadius) / speed.norm).toInt
  def nextPosition = position + speed

  def distanceToPod(other: Pod) =
    (other.position - position).norm - 2 * podRadius
  def collisionTime(other: Pod): Option[Double] = {
    val squareDist = (position).squareDistanceTo(other.position)
    val dist = (position).distanceTo(other.position)
    val squareRadii =
      (podRadius + other.podRadius) * (podRadius + other.podRadius)
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
      val length = sqrt(vx * vx + vy * vy)
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

      assert(t >= 0)
      assert(t <= 1.0)
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
    val product = n.x * dv.x + n.y * dv.y
    val f = n * (product / (nxnysquare * mcoeff))

    // We apply the impact vector once
    val newSpeed = speed - (f * (1.0 / m1))
    val otherNewSpeed = other.speed + (f * (1.0 / m2))

    // If the norm of the impact vector is less than 120, we normalize it to 120
    val impulse = sqrt(f.x * f.x + f.y * f.y)
    val newF = if (f.norm < 120.0) f.normalize * 120.0 else f

    // We apply the impact vector a second time
    val newNewSpeed = newSpeed - (f * (1.0 / m1))
    val newOtherNewSpeed = otherNewSpeed + (f * (1.0 / m2))

    Pod(
      position,
      destinations,
      orientation,
      newNewSpeed,
      steps,
      boostAvailable,
      hasShield
    )
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
    } else false
  }

  lazy val distance = position.distanceTo(destination)

  def nextDestination(race: Race): Point = race.nextCheckpoint(destination)

  lazy val destinationDirection = (destination - position).normalize

  def distanceToFinish: Double = {
    case class Dist(p: Point, d: Double)
    destinations
      .foldLeft(Dist(position, 0)) { case (d: Dist, next: Point) =>
        Dist(next, d.p.distanceTo(next))
      }
      .d
  }

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

  def hasReachDestination: Boolean =
    !hasFinished && ((position - destination).norm <= checkpointRadius)
  def hasFinished: Boolean = destinations.isEmpty
  def hasLost: Boolean = steps >= 100
  def hasWin: Option[Boolean] =
    if (hasFinished) Some(true) else if (hasLost) Some(false) else None
  def score = destinations.length

  def setPosition(newPos: Point) = Pod(
    newPos,
    destinations,
    orientation,
    speed,
    steps,
    boostAvailable,
    hasShield
  )

  def updateDestination: Pod = if (hasReachDestination)
    Pod(
      position,
      destinations.tail,
      orientation,
      speed,
      0,
      boostAvailable,
      hasShield
    )
  else this

  def updateEnd = Pod(
    position.round,
    destinations,
    orientation,
    (speed * 0.85).floor,
    steps + 1,
    boostAvailable,
    hasShield
  )
  def updatePosition(time: Double) = setPosition(
    position + (speed * time)
  ).updateDestination

  def update(command: Move) = updateSpeed(command).updatePosition(1.0).updateEnd
  def updateSpeed(command: Move): Pod = {
    if (command.thrust == Pilot.boost && boostAvailable == false)
      return Pod(
        position,
        destinations,
        orientation,
        speed,
        1000,
        boostAvailable,
        hasShield
      )
    val thrust =
      if (command.thrust == Pilot.boost) 650
      else if (command.thrust == Pilot.shield) 0
      else max(0, min(200, command.thrust))
    val hasBoost = if (command.thrust == Pilot.boost) false else boostAvailable
    val newShield = if (command.thrust == Pilot.shield) true else false
    val expectedOrientation = (command.direction - position).normalize
    val desiredOrientation = orientation.rotate(
      Degree(
        max(-18, min(18, expectedOrientation.radianWith(orientation).degree))
      )
    )
    val newSpeed = speed + desiredOrientation * thrust
    val angle =
      Angle.fromDegree(math.round(desiredOrientation.angleToEast.degree))
    val newOrientation = Point(1, 0).rotate(-angle)
    Pod(
      position,
      destinations,
      newOrientation,
      newSpeed,
      steps,
      hasBoost,
      newShield
    )
  }

  def updateWith(u: PodUpdate): Pod = {
    val dests =
      if (u.destination == destination) destinations else destinations.tail
    Pod(
      u.position,
      dests,
      u.orientation,
      u.speed,
      steps,
      boostAvailable,
      hasShield
    )
  }
}

object Pod {

  def apply(p: Point, d: List[Point], o: Point, s: Point): Pod =
    new Pod(p, d, o, s, 0, true, false)

  def distanceToLine(
      linePoint: Point,
      lineDirection: Point,
      testPoint: Point
  ): Double =
    List(linePoint, lineDirection, testPoint) match {
      case List(Point(x, y), Point(u, v), Point(a, b)) ⇒
        abs(v * a - u * b - v * x + u * y) / sqrt(v * v + u * u)
    }
}

case class Race(
    val pods: List[Pod],
    val checkpoints: List[Point],
    val laps: Int
) {

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
  else if (a.distance < b.distance) a
  else b

  def myLeader = compareScore(pod0, pod1)
  def enemyLeader = compareScore(enemy0, enemy1)

  def scoreMin = pods.map(_.score).min
  def isFirstTurn =
    if (scoreMin >= checkpoints.size * (laps - 1)) true else false
  def isLastTurn = if (scoreMin <= checkpoints.size) true else false

  def looser: Option[Pod] = pods.find(_.hasLost)
  def winner: Option[Pod] = pods.find(_.hasFinished)
  def winnerIsPlayerB: Boolean = inverted.winnerIsPlayerA
  def winnerIsPlayerA: Boolean = winner match {
    case Some(win) => myPods.exists(_ == win)
    case None =>
      looser match {
        case Some(los) => enemies.exists(_ == los)
        case None      => false
      }
  }
  def isFinished = looser.isDefined || winner.isDefined

  def raceDistance: Double = {
    case class Dist(p: Point, d: Double)
    checkpoints
      .foldLeft(Dist(checkpoints.last, 0)) { case (d: Dist, next: Point) =>
        Dist(next, d.p.distanceTo(next))
      }
      .d * laps
  }

  def checkpointIndex(p: Point) = checkpoints.zipWithIndex
    .filter { case (target, index) ⇒
      target == p
    }
    .head
    ._2

  lazy val boostCheckpoint: Point = {
    val loop = checkpoints ::: List(checkpoints.head)
    val maxDist = loop
      .sliding(2)
      .toList
      .map { case List(a, b) ⇒
        a.distanceTo(b)
      }
      .max
    loop
      .sliding(2)
      .filter { case List(a, b) ⇒
        a.distanceTo(b) == maxDist
      }
      .toList
      .head(1)
  }

  def previousCheckpoint(p: Point): Point = {
    val loop = checkpoints ::: List(checkpoints.head)
    val found: List[Point] = loop
      .sliding(2)
      .toList
      .filter { case List(a, b) ⇒
        b == p
      }
      .map { case List(a, b) ⇒
        a
      }
    if (!found.isEmpty) {
      val target = found.head
      target
    } else {
      // Print("can not find next checkpoint to ", p)
      // checkpoints.zipWithIndex.foreach(Print(_))
      Point(0, 0)
    }
  }
  def nextCheckpoint(p: Point): Point = {
    val loop = checkpoints ::: List(checkpoints.head)
    val found: List[Point] = loop
      .sliding(2)
      .toList
      .filter { case List(a, b) ⇒
        a == p
      }
      .map { case List(a, b) ⇒
        b
      }
    if (!found.isEmpty) {
      val target = found.head
      target
    } else {
      // Print("can not find next checkpoint to ", p)
      // checkpoints.zipWithIndex.foreach(Print(_))
      Point(0, 0)
    }
  }

  def simulate(commands: List[Move]): Race =
    simulateMoves(commands).simulateCollision.simulateEndOfTurn

  def simulateEndOfTurn = Race(pods.map(_.updateEnd), checkpoints, laps)

  def simulateMoves(commands: List[Move]): Race = {
    val p = pods.zip(commands).map { case (pod: Pod, c: Move) ⇒
      pod.updateSpeed(c)
    }
    Race(p, checkpoints, laps)
  }

  def simulateCollision: Race = {
    case class Collision(pod: Pod, time: Double)
    case class Motion(pod: Pod, time: Double)
    val p = pods.map(pod => {
      val collisions: List[Collision] = pods.filter(p => p != pod).flatMap {
        other => pod.collisionTime(other).map(t => Collision(other, t))
      }
      val motion: Motion = collisions
        .sortBy(_.time)
        .foldLeft(Motion(pod, 0.0)) { case (m: Motion, c: Collision) =>
          // move the two pod up to the collision time, then
          // compute the new speed due to the collision
          Motion(
            m.pod
              .updatePosition(c.time - m.time)
              .bounce(c.pod.updatePosition(c.time)),
            c.time
          )
        }
      motion.pod.updatePosition(1.0 - motion.time)
    })
    Race(p, checkpoints, laps)
  }
}

object Race {

  def random: Race = {
    import scala.util.Random
    def width = 16000
    def margin = 1000
    def height = 9000
    def checkpointsMinDistance = 4000
    def randomWidth = margin + Random.nextInt(width - 2 * margin)
    def randomHeight = margin + Random.nextInt(height - 2 * margin)
    def randomPosition = Point(randomWidth, -randomHeight)
    def randomPositions: Stream[Point] = Stream.continually(randomPosition)
    def checkpointsNb = 3 + Random.nextInt(3)
    def filterCheckpoints(n: Int, stream: Stream[Point]): Stream[Point] = if (
      n == 0
    ) stream
    else
      Stream.cons(
        stream.head,
        filterCheckpoints(
          n - 1,
          stream.tail.filter(p =>
            p.distanceTo(stream.head) > checkpointsMinDistance
          )
        )
      )
    def checkpoints: List[Point] = filterCheckpoints(
      checkpointsNb + 1,
      randomPositions
    ).take(checkpointsNb).toList

    def laps = 3 + Random.nextInt(3)
    Race(checkpoints, laps)
  }

  def parseInput(input: Stream[String]): Race = {
    def parsePods(
        checkpoints: List[Point],
        input: Stream[String]
    ): List[Pod] = {
      val updater = PodUpdater(checkpoints)
      val destinations = (for (i ← 1 to checkpoints.size)
        yield checkpoints.tail :+ checkpoints.head).flatten.toList
      input
        .take(4)
        .map { (line: String) =>
          {
            val u = updater.parsePodUpdate(line)
            Pod(
              u.position,
              destinations,
              (destinations.head - u.position).normalize,
              u.speed
            )
          }
        }
        .toList
    }
    val laps = input.head.toInt
    val checkpointsNb = input.tail.head.toInt
    val checkpoints = input.tail.tail
      .take(checkpointsNb)
      .map { (line: String) =>
        {
          val Array(checkpointX, checkpointY) = line.split(" ").map(_.toInt)
          Point(checkpointX, -checkpointY)
        }
      }
      .toList
    val pods = parsePods(checkpoints, input.drop(2 + checkpointsNb))
    Race(pods, checkpoints, laps)
  }

  def apply(checkpoints: List[Point], laps: Int): Race = {
    val checkpointsShift = checkpoints.tail :+ checkpoints.head
    val destinations =
      (for (i ← 0 to laps) yield checkpointsShift).flatten.toList
    val departLine =
      (checkpoints(1) - checkpoints(0)).rotate(Degree(90)).normalize
    val positions =
      List(1, 3, -1, -3).map(pos ⇒ checkpoints(0) + departLine * (pos * 450))
    val pods = positions.map(p ⇒
      Pod(p, destinations, (checkpoints(1) - p).normalize, Point(0, 0))
    )
    Race(pods, checkpoints, laps)
  }

}

case class PodUpdate(
    position: Point,
    destination: Point,
    orientation: Point,
    speed: Point
) {
  def pod: Pod = Pod(position, List(destination), orientation, speed)
}

case class Record(pod: Pod, command: Option[Move]) {
  override def toString = s"Record($pod, $command)"

  def data: Array[Double] = command match {
    case None    => pod.data ++ Array(0.0, 0.0, 0.0)
    case Some(c) => pod.data ++ c.data
  }
}

case class RaceRecord(
    laps: Int,
    checkpoints: List[Point],
    steps: List[List[Record]]
) {
  def updateWith(race: Race, commands: List[Option[Move]]) = {
    val record = List(
      Record(race.pods(0), commands(0)),
      Record(race.pods(1), commands(1)),
      Record(race.pods(2), commands(3)),
      Record(race.pods(3), commands(3))
    )
    RaceRecord(laps, checkpoints, steps ::: List(record))
  }
  def dump(): Unit = Print(toString)
  override def toString = s"RaceRecord($laps, $checkpoints, $steps)"

  def data: Array[Double] = {
    val a: Array[Double] =
      checkpoints.map(_.data).flatten.toArray ++ steps.flatten
        .map(_.data)
        .flatten
    Array(laps.toDouble, checkpoints.size) ++ a
  }

  def step(i: Int): Race = Race(steps(i).map(_.pod).toList, checkpoints, laps)
  def stepMoves(i: Int): List[Option[Move]] = steps(i).map(_.command).toList
}

case class PodUpdater(checkpoints: List[Point]) {
  def parsePodUpdate(line: String): PodUpdate = {
    val Array(x, y, vx, vy, angle, index) =
      for (i ← line split " ") yield i.toInt
    // reverse Y coordinate as the input are non cartesian
    val position = Point(x, -y)
    val destination = checkpoints(index)
    val orientation = Point(1, 0).rotate(Degree(-angle))
    val speed = Point(vx, -vy)
    PodUpdate(position, destination, orientation, speed)
  }
}
