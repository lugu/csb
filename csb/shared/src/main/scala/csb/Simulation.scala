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

case class Challenge(param: MetaParameter, races: Seq[Race]) {
  def judge = JudgeSimulation
  def debug(player: Player, racesNb: Int): Unit = {
    val gameSeq = Challenge(racesNb, param).games(player)
    val victories = gameSeq.count(g => g.winnerIsPlayerA)
    val failures = gameSeq.count(g => g.winnerIsPlayerB)
    val draw = gameSeq.count(g => (!g.winnerIsPlayerA) && (!g.winnerIsPlayerB))
    Print(s"Victories: $victories")
    Print(s"Failures: $failures" )
    Print(s"Draw: $draw" )
  }
  def games(player: Player) = races.map{ race => {
    Game(race, player, param.defaultPlayer, judge, 0).play
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
  def apply(racesNb: Int, param: MetaParameter): Challenge = Challenge(param, for (i <- 1 to racesNb) yield Race.random)
}

case class Individual(config: Config, fitness: Double) {
  def this(config: Config, player: Player, challenge: Challenge) {
    this(config, challenge.fitness(player))
  }
  def this(config: Config, challenge: Challenge) {
    this(config, MetaPlayer(config), challenge)
  }
  def updateFitness(challenge: Challenge) = new Individual(config, challenge)
  def player = MetaPlayer(config)
}

case class Population(p: Seq[Individual]) {
  def leader: Individual = p.maxBy(_.fitness)
  def selectPopulation(take: Int): Population = Population(p.sortBy(- _.fitness).take(take))
  def pickRandomIndividual = p(scala.util.Random.nextInt(p.size))
  def updateFitness(c: Challenge) = Population(p.map(_.updateFitness(c)))
  def breedPopulation(expectedSize: Int, c: Challenge): Population = {
    val parList = (1 to (expectedSize - p.size)).par
    val newGeneration = parList.map(i => breedIndividual(c))
    val newPopulation = Population(updateFitness(c).p ++ newGeneration.toSeq)
    if (c.param.debug) c.debug(newPopulation.leader.player, 10)
    newPopulation
  }
  def breedIndividual(c: Challenge) = {
    val newConfig = pickRandomIndividual.config.mergeWith(pickRandomIndividual.config).mutate(c.param.mutationRate)
    new Individual(newConfig, c)
  }
  def nextGeneration(c: Challenge): Population = {
    selectPopulation(c.param.selectionSize).breedPopulation(p.size, c)
  }

  @scala.annotation.tailrec
  final def afterNGenerations(n: Int, c: Challenge): Population = {
    Print(s"generation $n, best fitness: ${leader.fitness}")
    if (n == 0) this else nextGeneration(c).afterNGenerations(n - 1, c)
  }
}

// 1. generate population
// 2. evaluate population fitness
// 3. repeat:
// 3.1. select best-fit individuals
// 3.2. breed new individuals through cross-over and mutations
// 3.3. evaluate fitness of new individuals (population)
// 3.4. replace less-fit population with new individuals
case class Simulation(p: Population, c: Challenge) {

  def param = c.param

  def run() {
    Print("Starting simulation.")
    val leader = p.afterNGenerations(param.numberOfGeneration, c).leader
    Print("Simulation completed.")
    Print(leader.config)
  }
}

object Simulation extends App {

  val param: MetaParameter = Params
  val challenge = Challenge(param.numberOfRaces, param)

  def population(size: Int): Population = {
    def newIndividual(c: Challenge) = new Individual(param.baseConfig.randomize, c)
    def defaultIndividual(c: Challenge) = new Individual(param.baseConfig, c)
    val parList = (1 to size).par
    val individuals = parList.map(i => newIndividual(challenge)).toList
    Population(defaultIndividual(challenge) +: individuals)
  }
  (new Simulation(population(param.populationSize), challenge)).run()
}
