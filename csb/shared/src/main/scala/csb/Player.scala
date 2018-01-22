package csb
import scala.util.Random

case class Config(val p: Map[String,Int]) {
  val maxSpeed = p("maxSpeed")
  val maxSpeedCorrected = p("maxSpeedCorrected")
  val minSpeedCorrected = p("minSpeedCorrected")
  val maxAngleCorrected = p("maxAngleCorrected")
  val minAngleCorrected = p("minAngleCorrected")
  val reactionDistanceCorrected = p("reactionDistanceCorrected")
  val speedFactorNumCorrected = p("speedFactorNumCorrected")
  val speedFactorDenumCorrected = if (p("speedFactorDenumCorrected") == 0) 1 else p("speedFactorDenumCorrected")
  val speedFactorCorrected: Double = speedFactorNumCorrected.toDouble / speedFactorDenumCorrected 
  val minSpeedOffsetCorrected = p("minSpeedOffsetCorrected")
  val longDistanceBoost = p("longDistanceBoost")
  val smallAngleBoost = p("smallAngleBoost")
  val skipAngle = p("skipAngle")
  val skipDistance = p("skipDistance")
  val skipSpeed = p("skipSpeed")
  val choosePilot1Distance = p("choosePilot1Distance")
  val speedPilotHit = p("speedPilotHit")
  val smallDistancePilotWait = p("smallDistancePilotWait")
  val maxSpeedPilotWait = p("maxSpeedPilotWait")
  val anglePilotAvoid = p("anglePilotAvoid")
  val maxSpeedPilotAvoid = p("maxSpeedPilotAvoid")
  val largeDistancePilotFight = p("largeDistancePilotFight")
  val skipAnglePilot2 = p("skipAnglePilot2")
  val skipSpeedPilot2 = p("skipSpeedPilot2")
  val stepsToDestinationPilot2 = p("stepsToDestinationPilot2")
  val maxSpeedPilot2 = p("maxSpeedPilot2")
  val ratioPilot1 = if (p("ratioPilot1") == 0) 1 else p("ratioPilot1")
  val angleDenumPilot1 = if (p("angleDenumPilot1") == 0) 1 else p("angleDenumPilot1")
  val maxSpeedPilot1 = p("maxSpeedPilot1")
  val maxSpeedPilot0 = p("maxSpeedPilot0")

  def randomize: Config = {
    val n = p.mapValues(e => java.lang.Math.round(e + Random.nextGaussian() * e * 0.1).toInt)
    Print(n)
    Config(n)
  }
}

object DefaultConfig extends Config(Map( 
    "maxSpeed" -> 200,
    "maxSpeedCorrected" -> 200,
    "minSpeedCorrected" -> 60,
    "maxAngleCorrected" -> 18,
    "minAngleCorrected" -> 90,
    "reactionDistanceCorrected" -> 5000,
    "speedFactorNumCorrected" -> 10,
    "speedFactorDenumCorrected" -> 20,
    "minSpeedOffsetCorrected" -> 150,
    "longDistanceBoost" -> 4000, 
    "smallAngleBoost" -> 30,
    "skipAngle" -> 30,
    "skipDistance" -> 2000,
    "skipSpeed" -> 350,
    "choosePilot1Distance" -> 5000,
    "speedPilotHit" -> 200,
    "smallDistancePilotWait" -> 2500,
    "maxSpeedPilotWait" -> 200,
    "anglePilotAvoid" -> 90,
    "maxSpeedPilotAvoid" -> 200,
    "largeDistancePilotFight" -> 3000,
    "skipAnglePilot2" -> 30,
    "skipSpeedPilot2" -> 250,
    "stepsToDestinationPilot2" -> 4,
    "maxSpeedPilot2" -> 200,
    "ratioPilot1" -> 2,
    "angleDenumPilot1" -> 4,
    "maxSpeedPilot1" -> 200,
    "maxSpeedPilot0" -> 200
  ))


trait Configurable {
  implicit val config = DefaultConfig.randomize
}

trait RandomConfig {
  implicit val config = DefaultConfig.randomize
}

trait Player {
  def commands(race: Race): List[Command]
}

trait PilotConstructor extends Player {
  def pilot(pod: Pod, race: Race): Pilot
}

trait SimplePlayer extends Player with PilotConstructor with Configurable {
  def commands(race: Race): List[Command] = {
    val pilots: List[Pilot] = List(pilot(race.pod0, race), pilot(race.pod1, race))
    pilots.map(_.command)
  }
}

class MetaPlayer extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = {
    MetaPilot(pod, race)
  }
}

class Player0 extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = Pilot0(pod)
}

class Player1 extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = Pilot1(pod, race)
}

class Player2 extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = Pilot2(pod, race)
}

class PlayerDefense extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = PilotDefense(pod)
}

