package csb

object RunSimulation extends App {
  Simulation.run()
}

object Simulation {

  // meta parameters
  val populationSize = 100
  val selectionSize = 10
  val numberOfGeneration = 10
  val numberOfRaces = 40
  val mutationRate = 0.1
  val baseConfig = DefaultConfig

  // 1. generate population
  // 2. evaluate population fitness
  // 3. repeat:
  // 3.1. select best-fit individuals
  // 3.2. breed new individuals through cross-over and mutations
  // 3.3. evaluate fitness of new individuals (population)
  // 3.4. replace less-fit population with new individuals

  val races = for (i <- 1 to numberOfRaces) yield Race.random
  val judge = JudgeSimulation()

  val defaultPlayer = MetaPlayer(baseConfig)

  // use sum(-log(race.step)) to maximize
  def fitness(player: Player): Double = races.map{ race => {
      val game = Game(race, player, defaultPlayer, judge, 0).play
      if (game.winnerIsPlayerA)
        - scala.math.log(game.step)
      else
        - scala.math.log(3000)
    }}.sum

  def run() {
    Print("Default fitness: " + defaultIndividual.fitness)
    Print("Starting simulation.")
    val leader = population(populationSize).generation(numberOfGeneration, selectionSize).leader
    Print("Simulation completed.")
    Print(leader.config)
  }

  case class Individual(config: Config, fitness: Double) {
    def this(config: Config, player: Player) {
      this(config, fitness(player))
    }
    def this(config: Config) {
      this(config, MetaPlayer(config))
    }
  }

  case class Population(p: Seq[Individual]) {
    def selectPopulation(take: Int): Population = {
      Print("> " + p.sortBy(- _.fitness).take(take).map(_.fitness).mkString(" "))
      Population(p.sortBy(- _.fitness).take(take))
    }

    def pickRandomIndividual = p(scala.util.Random.nextInt(p.size))
    def breedIndividual = {
      val newConfig = pickRandomIndividual.config.mergeWith(pickRandomIndividual.config).mutate(mutationRate)
        new Individual(newConfig)
    }

    def breedPopulation(expectedSize: Int): Population = {
      val parList = (1 to (expectedSize - p.size)).par
      val nextGeneration = parList.map(i => breedIndividual)
      Population(p ++ nextGeneration.toSeq)
    }
    def leader: Individual = selectPopulation(1).p.head
    def nextGeneration(take: Int): Population = selectPopulation(take).breedPopulation(p.size)

    @scala.annotation.tailrec
    final def generation(n: Int, take: Int): Population = {
      Print(s"generation $n, best fitness: ${leader.fitness}")
      if (n == 0) this else nextGeneration(take).generation(n - 1, take)
    }
  }

  def newIndividual = new Individual(baseConfig.randomize)
  def defaultIndividual = new Individual(baseConfig)
  def population(size: Int): Population = {
    val parList = (1 to size).par
    val individuals = parList.map(i => newIndividual).toList
    Population(defaultIndividual +: individuals)
  }
}
