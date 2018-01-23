package csb

object Simulation extends App {
  def defaultCheckpoints = List(Point(5655,-2567), Point(4093,-7460), Point(13488,-2344), Point(12948,-7255))
  def testRace: Race = new Race(defaultCheckpoints, 3)

  override def main(args: Array[String]) {
    println("Default fitness: " + defaultIndividual.fitness)
    println("Starting simulation.")
    val leader = population(100).generation(3, 100).leader
    println("Simulation completed.")
    println("Config: " + leader.config.p)
  }

  case class Individual(config: Config, fitness: Int) {
    def this(config: Config, player: Player) {
      this(config, Simulation(testRace, 0, player, player).run().step)
    }
    def this(config: Config) {
      this(config, MetaPlayer(config))
    }
    def evolve = new Individual(config.randomize)
  }

  case class Population(p: Seq[Individual]) {
    def selectPopulation(take: Int): Population = {
      Print("> " + p.sortBy(_.fitness).take(take).map(_.fitness).mkString(" "))
      Population(p.sortBy(_.fitness).take(take))
    }
    def resamplePopulation(size: Int): Population = {
      import scala.util.Random
      val newPopulation = Population(for (i <- 1 to (p.size - size)) yield p(Random.nextInt(p.size)))
      Population(p ++ newPopulation.p).evolve
    }
    def evolve= Population(p.map(_.evolve))
    def leader: Individual = selectPopulation(1).p.head
    def nextGeneration(take: Int): Population = selectPopulation(take).resamplePopulation(p.size)

    @scala.annotation.tailrec
    final def generation(n: Int, take: Int): Population = {
      Print(s"generation $n, best fitness: ${leader.fitness}")
      if (n == 0) this else nextGeneration(take).generation(n - 1, take)
    }
  }

  def newIndividual = new Individual(DefaultConfig.randomize)
  def defaultIndividual = new Individual(DefaultConfig)
  def population(size: Int): Population = {
    val individuals = for (i <- 1 to (size - 1)) yield newIndividual
    Population(defaultIndividual +: individuals)
  }
}

case class Simulation(val race: Race, val step: Int, playerA: Player, playerB: Player) {

  def commands = playerA.commands(race) ::: playerB.commands(race.inverted)
  def isFinished: Boolean = if (step >= 1000) true else race.isFinished

  def next: Simulation = {
    Simulation(race.simulate(commands), step + 1, playerA, playerB)
  }

  @scala.annotation.tailrec
  final def run(): Simulation = if (isFinished) this else next.run()
}
