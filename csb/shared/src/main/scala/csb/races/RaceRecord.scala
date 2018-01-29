package csb.races

trait RaceRecord {
  val record: String

  def input: Stream[String] = record.split("\n").filter(_.startsWith("in: ")).map(_.drop(4)).toStream
  def output: Stream[String] = record.split("\n").filter(_.startsWith("out: ")).map(_.drop(5)).toStream

  def race = csb.Race.parseInput(input)
  def playerA = csb.DummyPlayer()
  def playerB = csb.DummyPlayer()
  def judge = csb.JudgeReplay(input.drop(race.checkpoints.size + 6))
  def game = csb.Game(race, playerA, playerB, judge, 0)
}
