import csb.{Race,RepeatPlayer,BetterConfig,JudgeRepeat,DummyPlayer,MetaPlayer,Input,Output,Game,Print}

object Player extends App {
  val race = Race.parseInput(Input.stream)
  val playerA = RepeatPlayer(MetaPlayer(BetterConfig), Output.apply)
  val playerB = DummyPlayer()
  val judge = JudgeRepeat(() => { Input.stream })
  val game = Game(race, playerA, playerB, judge, 0).play

  game.race.winner match {
    case Some(pod) => Print("winner is: " + pod.toString)
    case None => Print("no winner")
  }
}
