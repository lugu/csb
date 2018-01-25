import csb.{Race,RaceRecord,RepeatPlayer,DefaultConfig,JudgeReplay,DummyPlayer,MetaPlayer,TestPlayer,IO,Input,Output,PodUpdater,Game}

object Player extends App {
  val race = Race.parseInput(Input.apply)
  val player = RepeatPlayer(new MetaPlayer(DefaultConfig), Output.apply)
  var game = Game(race, player, DummyPlayer(), JudgeReplay(Input.apply))
  game.play
}

