import csb.{Race,RaceRecord,MetaPlayer,TestPlayer,IO,Input,Output,PodUpdater}

object Player extends App {
  var race = Race.parseInput(Input.apply)
  val updater = PodUpdater(race.checkpoints)
  var recorder = race.newRecorder
  val player = new MetaPlayer(DefaultConfig)
  while (!race.isFinished) {
    val commands = player.commands(race)
    recorder = recorder.updateWith( race, List(Some(commands(0)), Some(commands(1)), None, None))
    // IO.dump()
    if (!race.isFinished) {
      commands.foreach(c => Output(c.answer))
      val pods = for (p ← race.pods) yield {
        p.updateWith(updater.parsePodUpdate(Input()))
      }
      race = Race(pods, race.checkpoints, race.laps)
    }
  }
}
