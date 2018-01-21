package csb.player

object Records {
  val checkpoints = List(Point(0, 0), Point(1, 1) * 1000, Point(2, 2) * 1000, Point(3, 3) * 1000)
  val pods = Pod(Point(0, 0), List(Point(0, 0)), Point(0, 0), Point(0, 0), true)
  val recorded = RaceRecord(1, checkpoints, List())

  def apply(args: Array[String]): RaceRecord = recorded
}

case class Simulation(record: RaceRecord) {
  def step(i: Int) = {
    val race = record.step(i)
    val commands = (new MetaPlayer).commands(race) ::: (new MetaPlayer).commands(race.inverted)
    race.pods.foreach(Print(_))
    race.simulate(commands)
  }
}

object Simulation extends App {
  val simulation = Simulation(Records(args))
  for (i <- 0 to 100) {
    Print("race step: " + i)
    simulation.step(i)
  }
}


