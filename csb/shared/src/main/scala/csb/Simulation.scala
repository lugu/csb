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

case class Environment(param: MetaParameter, races: Seq[Race]) {
  def judge = JudgeSimulation
  def debug(player: Player, racesNb: Int): Unit = {
    val gameSeq = Environment(racesNb, param).games(player)
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

object Environment {
  def apply(racesNb: Int, param: MetaParameter): Environment = Environment(param, for (i <- 1 to racesNb) yield Race.random)
}

case class Individual(config: Config, fitness: Double) {
  def this(config: Config, player: Player, environment: Environment) {
    this(config, environment.fitness(player))
  }
  def this(config: Config, environment: Environment) {
    this(config, MetaPlayer(config), environment)
  }
  def fitness(environment: Environment) = environment.fitness(player)
  def player = MetaPlayer(config)
}

case class Population(p: Seq[Individual]) {
  def leader: Individual = p.maxBy(_.fitness)
  private def selectPopulation(take: Int): Population = Population(p.sortBy(- _.fitness).take(take))
  private def randomIndividual = p(scala.util.Random.nextInt(p.size))
  private def updateFitness(e: Environment) = Population(p.map(i => Individual(i.config, i.fitness(e))))
  private def breedPopulation(expectedSize: Int, e: Environment): Population = {
    val parList = (1 to (expectedSize - p.size)).par
    val newGeneration = parList.map(i => breedIndividual(e))
    val newPopulation = Population(updateFitness(e).p ++ newGeneration.toSeq)
    if (e.param.debug) e.debug(newPopulation.leader.player, 10)
    newPopulation
  }
  private def breedIndividual(e: Environment) = {
    val newConfig = randomIndividual.config.mergeWith(randomIndividual.config).mutate(e.param.mutationRate)
    new Individual(newConfig, e)
  }
  def evolve(e: Environment): Population = {
    selectPopulation(e.param.selectionSize).breedPopulation(p.size, e)
  }

  def fitness(e: Environment): Double = leader.fitness(e)
}

// 1. generate population
// 2. evaluate population fitness
// 3. repeat:
// 3.1. select best-fit individuals
// 3.2. breed new individuals through cross-over and mutations
// 3.3. evaluate fitness of new individuals (population)
// 3.4. replace less-fit population with new individuals
case class Experiment(p: Population, e: Environment) {
  def evolve: Population = p.evolve(e)
  def run: Stream[Population] = Stream.continually(evolve)
  def fitness: Double = p.fitness(e)
}

object Simulation extends App {


  val param: MetaParameter = Params
  val environment = Environment(param.numberOfRaces, param)

  def population(size: Int): Population = {
    def newIndividual(e: Environment) = new Individual(param.baseConfig.randomize, e)
    def defaultIndividual(e: Environment) = new Individual(param.baseConfig, e)
    val parList = (1 to size).par
    val individuals = parList.map(i => newIndividual(environment)).toList
    Population(defaultIndividual(environment) +: individuals)
  }

  Print("Starting simulation.")
  val leader = Experiment(population(param.populationSize), environment).run.take(param.numberOfGeneration).last.leader
  Print("Experiment completed.")
  Print(leader.config)
}
