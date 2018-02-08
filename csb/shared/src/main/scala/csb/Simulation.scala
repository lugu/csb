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
  val defaultConfig = DefaultConfig

  // 1. generate population
  // 2. evaluate population fitness
  // 3. repeat:
  // 3.1. select best-fit individuals
  // 3.2. breed new individuals through cross-over and mutations
  // 3.3. evaluate fitness of new individuals (population)
  // 3.4. replace less-fit population with new individuals
  
  var debug = true

  case class Challenge(races: Seq[Race]) {
    def debug(player: Player, racesNb: Int): Unit = {
      val gameSeq = Challenge(racesNb).games(player)
      val victories = gameSeq.count(g => g.winnerIsPlayerA)
      val failures = gameSeq.count(g => g.winnerIsPlayerB)
      val draw = gameSeq.count(g => (!g.winnerIsPlayerA) && (!g.winnerIsPlayerB))
      Print(s"Victories: $victories")
      Print(s"Failures: $failures" )
      Print(s"Draw: $draw" )
    }
    def games(player: Player) = races.map{ race => {
        Game(race, player, defaultPlayer, judge, 0).play
      }}
    // use sum(-log(race.step)) to maximize
    def fitness(player: Player): Double = {
      val score = games(player).map(g =>
      if (g.winnerIsPlayerA)
        - scala.math.log(g.step)
      else
        - scala.math.log(10000)
      ).sum
      score
    }
  }
  object Challenge {
    def apply(): Challenge = Challenge(numberOfRaces)
    def apply(racesNb: Int): Challenge = Challenge(for (i <- 1 to racesNb) yield Race.random)
  }


  def newChallenge = Challenge(for (i <- 1 to numberOfRaces) yield Race.random)
  val judge = JudgeSimulation()

  val defaultPlayer = MetaPlayer(defaultConfig)

  def run() {
    Print("Starting simulation.")
    val leader = population(populationSize).afterNGenerations(numberOfGeneration).leader
    Print("Simulation completed.")
    Print(leader.config)
  }

  case class Individual(config: Config, fitness: Double)(implicit challenge: Challenge) {
    def this(config: Config, player: Player)(implicit challenge: Challenge) {
      this(config, challenge.fitness(player))
    }
    def this(config: Config)(implicit challenge: Challenge) {
      this(config, MetaPlayer(config))
    }
    def updateFitness(challenge: Challenge) = new Individual(config)(challenge)
    def player = MetaPlayer(config)
  }

  case class Population(p: Seq[Individual]) {
    def leader: Individual = p.maxBy(_.fitness)

    def nextGeneration: Population = {
      selectPopulation(selectionSize).breedPopulation(p.size)
    }

    // utils
    def updateFitness(c: Challenge) = Population(p.map(_.updateFitness(c)))
    def pickRandomIndividual = p(scala.util.Random.nextInt(p.size))
    def breedIndividual(c: Challenge) = {
      val newConfig = pickRandomIndividual.config.mergeWith(pickRandomIndividual.config).mutate(mutationRate)
      new Individual(newConfig)(c)
    }

    // 1. select the best individuals
    def selectPopulation(take: Int): Population = {
      val pop = Population(p.sortBy(- _.fitness).take(take))
      Print("> " + pop.p.map(_.fitness).mkString(" "))
      pop
    }


    // 2. mix individuals
    def breedPopulation(expectedSize: Int): Population = {
      val challenge = Challenge(numberOfRaces)
      val parList = (1 to (expectedSize - p.size)).par
      val newGeneration = parList.map(i => breedIndividual(challenge))
      val newPopulation = Population(updateFitness(challenge).p ++ newGeneration.toSeq)
      if (debug) challenge.debug(newPopulation.leader.player, 10)
      newPopulation
    }

    @scala.annotation.tailrec
    final def afterNGenerations(n: Int): Population = {
      Print(s"generation $n, best fitness: ${leader.fitness}")
      if (n == 0) this else nextGeneration.afterNGenerations(n - 1)
    }
  }

  def newIndividual(c: Challenge) = new Individual(baseConfig.randomize)(c)
  def defaultIndividual(c: Challenge) = new Individual(baseConfig)(c)
  def population(size: Int): Population = {
    val challenge = Challenge(numberOfRaces)
    val parList = (1 to size).par
    val individuals = parList.map(i => newIndividual(challenge)).toList
    Population(defaultIndividual(challenge) +: individuals)
  }
}
