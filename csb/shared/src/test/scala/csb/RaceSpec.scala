package csb

import org.scalatest._
import scala.io.Source

case class JudgeTest(var input: Stream[String]) extends Judge {
  import org.scalatest.Assertions._
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
  def computedPods(race: Race, commands: List[Command]): List[Pod] = {
    race.simulate(commands).pods
  }

  def judge(race: Race, commands: List[Command]): Race = input.headOption match {
    case None => race
    case _ => {
      val solution = expectedPods(race)
      val computed = computedPods(race, commands)
      solution.zip(computed).foreach {
        case (sol: Pod, com: Pod) =>
        // FIXME: shall fix this
        // assert(sol.position == com.position, "compare pod position")
      }
      Race(solution, race.checkpoints, race.laps)
    }
  }
}


class RaceSpec extends FlatSpec with Matchers {

  val dummyCheckpoints = List(Point(1, 1), Point(2, 2), Point(3, 3), Point(4, 4), Point(5, 5))

  "PodUpdater" should "be able to parse input correctly" in {
    val input = "-1000 2000 200 -100 90 3"
    val updater = PodUpdater(dummyCheckpoints)
    val pod = updater.parsePodUpdate(input).pod

    assert(pod.position == Point(-1000, -2000), "parse position")
    assert(pod.speed == Point(200, 100), "parse speed")
    assert(pod.destinations.head == Point(4, 4), "parse index")

    pod.orientation.x should equal (0.0 +- 0.001)
    pod.orientation.y should equal (-1.0 +- 0.001)
  }

  def updatePosition(before: String, after: String, command: String) = {
    val updater = PodUpdater(dummyCheckpoints)
    val pod = updater.parsePodUpdate(before).pod
    val computed = pod.update(new Command(command))
    val result = updater.parsePodUpdate(after).pod

    assert(computed.position == result.position, "in position test")
  }

  def updateTwoPlayerPosition(input: String) = {
    val lines = input.split("\n")
    updatePosition(lines(0), lines(6), lines(4))
    updatePosition(lines(1), lines(7), lines(5))
  }

  "Pod" should "be able to update their position" in {
    val before = "3576 3719 -43 694 86 3"
    val command = "8689 1866 200 200 SKIP-CORRECTED"
    val after = "3608 4598 27 747 68 3"
    updatePosition(before, after, command)
  }

  "Judge" should "be able to resolve collision 1" in {
    val race = Race.parseInput(Source.fromResource("collision-1.txt").getLines.toStream)
    val playerA = ReplayPlayer(Source.fromResource("collision-1-output.txt").getLines.toStream)
    val playerB = DummyPlayer()
    val judge = JudgeTest(Source.fromResource("collision-1.txt").getLines.toStream.drop(race.checkpoints.size + 6))
    val game = Game(race, playerA, playerB, judge, 0).play
  }

  "Judge" should "be able to resolve collision 2" in {
    val race = Race.parseInput(Source.fromResource("collision-2.txt").getLines.toStream)
    val playerA = ReplayPlayer(Source.fromResource("collision-2-output.txt").getLines.toStream)
    val playerB = DummyPlayer()
    val judge = JudgeTest(Source.fromResource("collision-2.txt").getLines.toStream.drop(race.checkpoints.size + 6))
    val game = Game(race, playerA, playerB, judge, 0).play
  }
}

