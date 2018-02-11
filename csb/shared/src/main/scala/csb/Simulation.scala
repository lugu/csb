package csb

trait MetaParameter {
  val populationSize: Int
  val selectionSize: Int
  val numberOfGeneration: Int
  val numberOfRaces: Int
  val mutationRate: Double
  val baseConfig: Config
  val defaultConfig: Config
  lazy val defaultPlayer = MetaPlayer(defaultConfig)
  val debug: Boolean
}

// meta parameters
object Params extends MetaParameter {
  val populationSize = 10
  val selectionSize = 1
  val numberOfGeneration = 10
  val numberOfRaces = 10
  val mutationRate = 0.1
  val baseConfig = DefaultConfig
  val defaultConfig = DefaultConfig
  val debug = false
}

case class Challenge(races: Seq[Race]) {
  def judge = JudgeSimulation
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
    Game(race, player, Params.defaultPlayer, judge, 0).play
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
  def apply(racesNb: Int): Challenge = Challenge(for (i <- 1 to racesNb) yield Race.random)
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

  def updateFitness(c: Challenge) = Population(p.map(_.updateFitness(c)))
  def breedPopulation(expectedSize: Int)(implicit param: MetaParameter): Population = {
    val challenge = Challenge(param.numberOfRaces)
    val parList = (1 to (expectedSize - p.size)).par
    val newGeneration = parList.map(i => breedIndividual(challenge))
    val newPopulation = Population(updateFitness(challenge).p ++ newGeneration.toSeq)
    if (param.debug) challenge.debug(newPopulation.leader.player, 10)
    newPopulation
  }
  def pickRandomIndividual = p(scala.util.Random.nextInt(p.size))
  def breedIndividual(c: Challenge)(implicit param: MetaParameter) = {
    val newConfig = pickRandomIndividual.config.mergeWith(pickRandomIndividual.config).mutate(param.mutationRate)
    new Individual(newConfig)(c)
  }
  def selectPopulation(take: Int): Population = Population(p.sortBy(- _.fitness).take(take))
  def nextGeneration(implicit param: MetaParameter): Population = {
    selectPopulation(param.selectionSize).breedPopulation(p.size)
  }


  @scala.annotation.tailrec
  final def afterNGenerations(n: Int)(implicit param: MetaParameter): Population = {
    Print(s"generation $n, best fitness: ${leader.fitness}")
    if (n == 0) this else nextGeneration.afterNGenerations(n - 1)
  }
}

// 1. generate population
// 2. evaluate population fitness
// 3. repeat:
// 3.1. select best-fit individuals
// 3.2. breed new individuals through cross-over and mutations
// 3.3. evaluate fitness of new individuals (population)
// 3.4. replace less-fit population with new individuals
case class Simulation(p: Population, c: Challenge, param: MetaParameter) {

  def evolve: Population = ???
  def evaluate: Double = ???

  def run() {
    Print("Starting simulation.")
    val leader = p.afterNGenerations(param.numberOfGeneration)(param).leader
    Print("Simulation completed.")
    Print(leader.config)
  }
}

object Simulation extends App {

  val param: MetaParameter = Params
  val challenge = Challenge(param.numberOfRaces)

  def population(size: Int): Population = {
    def newIndividual(c: Challenge) = new Individual(param.baseConfig.randomize)(c)
    def defaultIndividual(c: Challenge) = new Individual(param.baseConfig)(c)
    val parList = (1 to size).par
    val individuals = parList.map(i => newIndividual(challenge)).toList
    Population(defaultIndividual(challenge) +: individuals)
  }
  (new Simulation(population(param.populationSize), challenge, param)).run()
}
