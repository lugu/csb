package csb
import scala.util.Random

case class Config(val name: String, val p: Map[String,Int]) {
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

  def mergeWith(other: Config): Config = {
    Config(name + "/" + other.name, Map[String,Int]() ++ p.keys.map(k => (k, (p(k) + other.p(k)) / 2)))
  }
  def mutate(rate: Double): Config = {
    val n = p.mapValues(e => java.lang.Math.round(e + Random.nextGaussian() * e * rate).toInt)
    Config(s"mutate($name, $rate)", n)
  }
  def randomize: Config = mutate(0.1)

  override def toString: String = "Map(" +
    p.map{ case (k, v) => "\"" + k + "\"" + s" -> $v"}.mkString(",\n") +
    "))"
}

object DefaultConfig extends Config("DefaultConfig", Map(
  "angleDenumPilot1" -> 4,
  "anglePilotAvoid" -> 90,
  "choosePilot1Distance" -> 5000,
  "largeDistancePilotFight" -> 3000,
  "longDistanceBoost" -> 4000,
  "maxAngleCorrected" -> 18,
  "maxSpeed" -> 200,
  "maxSpeedCorrected" -> 200,
  "maxSpeedPilot0" -> 200,
  "maxSpeedPilot1" -> 200,
  "maxSpeedPilot2" -> 200,
  "maxSpeedPilotAvoid" -> 200,
  "maxSpeedPilotWait" -> 200,
  "minAngleCorrected" -> 90,
  "minSpeedCorrected" -> 60,
  "minSpeedOffsetCorrected" -> 150,
  "ratioPilot1" -> 2,
  "reactionDistanceCorrected" -> 5000,
  "skipAngle" -> 30,
  "skipAnglePilot2" -> 30,
  "skipDistance" -> 2000,
  "skipSpeed" -> 350,
  "skipSpeedPilot2" -> 250,
  "smallAngleBoost" -> 30,
  "smallDistancePilotWait" -> 2500,
  "speedFactorDenumCorrected" -> 20,
  "speedFactorNumCorrected" -> 10,
  "speedPilotHit" -> 200,
  "stepsToDestinationPilot2" -> 4
))

object BetterConfig extends Config("BetterConfig", 
  Map("maxSpeedPilot0" -> 209,
    "maxSpeedPilot1" -> 238,
    "anglePilotAvoid" -> 98,
    "maxSpeedCorrected" -> 197,
    "skipSpeedPilot2" -> 280,
    "minAngleCorrected" -> 78,
    "smallAngleBoost" -> 22,
    "skipAnglePilot2" -> 27,
    "skipSpeed" -> 263,
    "maxSpeedPilot2" -> 154,
    "stepsToDestinationPilot2" -> 2,
    "maxAngleCorrected" -> 17,
    "speedFactorDenumCorrected" -> 15,
    "skipAngle" -> 28,
    "angleDenumPilot1" -> 3,
    "reactionDistanceCorrected" -> 5716,
    "skipDistance" -> 2289,
    "longDistanceBoost" -> 3602,
    "maxSpeedPilotAvoid" -> 160,
    "maxSpeed" -> 210,
    "speedPilotHit" -> 248,
    "maxSpeedPilotWait" -> 246,
    "speedFactorNumCorrected" -> 10,
    "largeDistancePilotFight" -> 2457,
    "ratioPilot1" -> 1,
    "minSpeedOffsetCorrected" -> 239,
    "smallDistancePilotWait" -> 2965,
    "choosePilot1Distance" -> 5292,
    "minSpeedCorrected" -> 53))

object BestConfig extends Config("BestConfig", 
  Map("maxSpeedPilot0" -> 199,
    "maxSpeedPilot1" -> 298,
    "anglePilotAvoid" -> 95,
    "maxSpeedCorrected" -> 242,
    "skipSpeedPilot2" -> 256,
    "minAngleCorrected" -> 78,
    "smallAngleBoost" -> 18,
    "skipAnglePilot2" -> 22,
    "skipSpeed" -> 330,
    "maxSpeedPilot2" -> 115,
    "stepsToDestinationPilot2" -> 1,
    "maxAngleCorrected" -> 16,
    "speedFactorDenumCorrected" -> 10,
    "skipAngle" -> 19,
    "angleDenumPilot1" -> 2,
    "reactionDistanceCorrected" -> 4957,
    "skipDistance" -> 2771,
    "longDistanceBoost" -> 3652,
    "maxSpeedPilotAvoid" -> 141,
    "maxSpeed" -> 220,
    "speedPilotHit" -> 165,
    "maxSpeedPilotWait" -> 334,
    "speedFactorNumCorrected" -> 7,
    "largeDistancePilotFight" -> 2150,
    "ratioPilot1" -> 1,
    "minSpeedOffsetCorrected" -> 234,
    "smallDistancePilotWait" -> 3041,
    "choosePilot1Distance" -> 6458,
    "minSpeedCorrected" -> 50))



trait Configurable {
  implicit val config = DefaultConfig
}

trait RandomConfig {
  implicit val config = DefaultConfig.randomize
}

trait Player {
  def name: String
  def commands(race: Race): List[Move]
}

trait PilotConstructor extends Player {
  def pilot(pod: Pod, race: Race): Pilot
}

trait SimplePlayer extends Player with PilotConstructor {
  def commands(race: Race): List[Move] = {
    // Print(name)
    val pilots: List[Pilot] = List(pilot(race.pod0, race), pilot(race.pod1, race))
    pilots.map(_.command)
  }
}

case class MetaPlayer(val config: Config) extends SimplePlayer {
  def name = config.name
  def pilot(pod: Pod, race: Race): Pilot = {
    MetaPilot(pod, race)(config)
  }
}

case class RepeatPlayer(val player: Player, val output: (String) => Unit) extends Player {
  def name = "RepeatPlayer"
  def commands(race: Race): List[Move] = {
    val c = player.commands(race)
    c.foreach(a => output(a.answer))
    c
  }
}

case class TestPlayer(var step: Int) extends Player {
  def name = "TestPlayer"
  def commands(race: Race): List[Move] = {
    step += 1
    if (step < 100) List(Move(Point(0, 100), 10, "before 1"), Move(Point(1000, 100), 10, "before 2"))
    else List(Move(race.pod1.position, 100, "boom 1"), Move(Point(1000, 100), 10, "waiting 2"))
  }
}

case class ReplayPlayer(var input: Stream[String]) extends Player {
  def name = "ReplayPlayer"
  def command = {
    val c = new Move(input.head)
    input = input.tail
    c
  }
  def commands(race: Race): List[Move] = List(command, command)
}

case class DummyPlayer() extends Player {
  def name = "DummyPlayer"
  def command(pos: Point) = Move(pos, 0, "dummy")
  def commands(race: Race): List[Move] = List(command(race.pod0.position), command(race.pod1.position))
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

trait Judge {
  def judge(race: Race, commands: List[Move]): Race
  def isFinished(game: Game): Boolean = if (game.step > 3000) true else game.race.isFinished
}

object JudgeSimulation extends Judge {
  def judge(race: Race, commands: List[Move]) = race.simulate(commands)
}

case class JudgeReplay(var input: Stream[String]) extends Judge {
  override def isFinished(game: Game): Boolean = input.isEmpty
  def judge(race: Race, commands: List[Move]): Race = input.headOption match {
    case None => race
    case _ => {
      val updater = PodUpdater(race.checkpoints)
      val pods = input
        .take(4)
        .zip(race.pods)
        .map {
          case (line: String, pod: Pod) =>
            pod.updateWith(updater.parsePodUpdate(line))
        }.toList
      input = input.drop(4)
      Race(pods, race.checkpoints, race.laps)
    }
  }
}

case class JudgeRepeat(val input: () => Stream[String]) extends Judge {
  def judge(race: Race, commands: List[Move]) = {
    val updater = PodUpdater(race.checkpoints)
    val pods = input()
      .take(4)
      .zip(race.pods)
      .map {
        case (line: String, pod: Pod) =>
          pod.updateWith(updater.parsePodUpdate(line))
      }
        .toList
        Race(pods, race.checkpoints, race.laps)
  }
}

case class JudgeTest(var input: Stream[String]) extends Judge {

  case class ComparePod(pod: Pod, reference: Pod) {

    def testPoint(clue: String, point: Point, reference: Point, tolerance: Double) {
      testValue(clue + ".x", point.x, reference.x, tolerance)
      testValue(clue + ".y", point.y, reference.y, tolerance)
    }

    def testValue(clue: String, value: Double, reference: Double, tolerance: Double) {
      if (value < reference - tolerance || value > reference + tolerance)
        if (IO.verbose) Print(s"test $clue: $value should be $reference +/- $tolerance")
      /*
      import org.scalatest._
      import Matchers._
      try {
        withClue(s"test $clue:") {
          value should equal (reference +- tolerance)
        }
      } catch {
        case (e: org.scalatest.exceptions.TestFailedException) => {
          Print(e.getMessage)
        }
      }
      */
    }
    def test = {
      testValue("orientation", pod.orientation.angleToEast.degree, reference.orientation.angleToEast.degree, 1)
      testPoint("speed", pod.speed, reference.speed, 1)
      testPoint("position", pod.position, reference.position, 1)
    }
  }


  override def isFinished(game: Game): Boolean = input.isEmpty
  def expectedPods(race: Race): List[Pod] = {
    val updater = PodUpdater(race.checkpoints)
    val expected = input
      .take(4)
      .map { line => updater.parsePodUpdate(line).pod }
      .toList
    input = input.drop(4)
    expected
  }
  def computedPods(race: Race, commands: List[Move]): List[Pod] = {
    race.simulate(commands).pods
  }

  def judge(race: Race, commands: List[Move]): Race = input.headOption match {
    case None => race
    case _ => {
      val solution = expectedPods(race)
      val computed = computedPods(race, commands)
      solution.take(2).zip(computed.take(2)).foreach {
        case (sol: Pod, com: Pod) => ComparePod(com, sol).test
      }
      Race(solution, race.checkpoints, race.laps)
    }
  }
}


case class Game(race: Race, playerA: Player, playerB: Player, judge: Judge, step: Int) {
  def winner(game: Game): Option[Pod] = game.race.winner
  def isFinished = judge.isFinished(this)
  def winnerIsPlayerB: Boolean = race.winnerIsPlayerB
  def winnerIsPlayerA: Boolean = race.winnerIsPlayerA
  def nextTurn: Game = {
    IO.debug()
    val isFinished = judge.isFinished(this)
    val commands = playerA.commands(race) ::: playerB.commands(race.inverted)
    Game(judge.judge(race, commands), playerA, playerB, judge, step + 1)
  }
  @scala.annotation.tailrec
  final def play: Game = if (judge.isFinished(this)) this else nextTurn.play
}


object Input {
  def stream: Stream[String] = Stream.continually(IO.readLine())
}

object Output {
  def apply(s: String) = IO.println(s)
}

object IO {

  var record = ""

  val verbose = false
  def debug() = if (verbose) record.split("\n").foreach(s => Print(s))

  def println(out: String) = {
    record += out.split("\n").map(s => s"out: $s").mkString("\n") + "\n"
    scala.Console.println(out)
  }

  def readLine(): String = {
    val line = scala.io.StdIn.readLine()
    record += s"in: $line\n"
    line
  }

  def decompress(inData: Array[Byte]): Array[Byte] = {
    import java.util.zip.Inflater
    val inflater = new Inflater()
    inflater.setInput(inData)
    val decompressedData = new Array[Byte](inData.size * 2)
    var count = inflater.inflate(decompressedData)
    var finalData = decompressedData.take(count)
    while (count > 0) {
      count = inflater.inflate(decompressedData)
      finalData = finalData ++ decompressedData.take(count)
    }
    return finalData
  }

  def compress(inData: Array[Byte]): Array[Byte] = {
    import java.util.zip.Deflater
    var deflater: Deflater = new Deflater()
    deflater.setInput(inData)
    deflater.finish
    // compressed data can be larger than original data
    val compressedData = new Array[Byte](inData.size * 2)
    val count: Int = deflater.deflate(compressedData)
    return compressedData.take(count)
  }

  def unbase64(inString: String): Array[Byte] = {
      import java.util.Base64
      Base64.getDecoder.decode(inString)
  }

  def base64(inData: Array[Byte]): String = {
      import java.util.Base64
      Base64.getEncoder.encodeToString(inData)
  }

  def dump() = {
    val out = base64(compress(record.getBytes))
    var printed = 0
    while(printed < out.size) {
        Print(out.slice(printed, printed + 1024))
        printed += 1024
    }
  }
}
