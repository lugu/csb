package csb

object RunSimulation extends App {
  Simulation.run()
}

object Simulation {

  def fitness(player: Player): Int = {
    def judge = JudgeSimulation()
    def race: Race = Race.random
    def game(player: Player): Game = Game(race, player, player, judge, 0)

    game(player).play.step
  }

  def run() {
    println("Default fitness: " + defaultIndividual.fitness)
    println("Starting simulation.")
    val leader = population(100).generation(3, 100).leader
    println("Simulation completed.")
    println("Config: " + leader.config.p)
  }

  case class Individual(config: Config, fitness: Int) {
    def this(config: Config, player: Player) {
      this(config, fitness(player))
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
