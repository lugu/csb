package csb

object Simulation extends App {
  def defaultPlayer() = new MetaPlayer(DefaultConfig.randomize)
  def defaultCheckpoints = List(Point(5655,-2567), Point(4093,-7460), Point(13488,-2344), Point(12948,-7255))
  def apply(): Simulation = Simulation(new Race(defaultCheckpoints, 3), 0, defaultPlayer(), defaultPlayer())

  println("Starting simulation.")
  val sim = Simulation().run()
  println("Simulation completed.")
}

case class Simulation(val race: Race, val step: Int, playerA: Player, playerB: Player) {

  def commands = playerA.commands(race) ::: playerB.commands(race.inverted)
  def isFinished: Boolean = if (step > 3000) true else race.isFinished

  def next: Simulation = {
    Print("race step: " + step)
    Simulation(race.simulate(commands), step + 1, playerA, playerB)
  }

  @scala.annotation.tailrec
  final def run(): Simulation = if (isFinished) this else next.run()
}
