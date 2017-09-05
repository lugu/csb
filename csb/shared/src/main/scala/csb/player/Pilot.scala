package csb.player

import math.{ abs, min, max, sin, Pi }

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
  override def toString = s"Command($direction,$thrust," + "\"" + label + "\")"
  def data = direction.data ++ Array(thrust)
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
    // Print("dis " + dist)
    // Print("thrust " + t)
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

