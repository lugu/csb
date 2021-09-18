package sim

import csb.{
  Config,
  DefaultConfig,
  Game,
  JudgeSimulation,
  MetaPlayer,
  Output,
  Pilot1Player,
  Player,
  Print,
  Race
}

case class MetaParameter(
    populationSize: Int,
    selectionSize: Int,
    numberOfGeneration: Int,
    trainRacesNb: Int,
    testRacesNb: Int,
    mutationRate: Double,
    baseConfig: Config,
    defaultConfig: Config,
    notRandom: Boolean,
    randomRaces: Boolean,
    failEarly: Boolean,
    debug: Boolean
) {
  lazy val defaultPlayer = MetaPlayer(defaultConfig)

  def setPopulation(n: Int) = MetaParameter(
    n,
    selectionSize,
    numberOfGeneration,
    trainRacesNb,
    testRacesNb,
    mutationRate,
    baseConfig,
    defaultConfig,
    notRandom,
    randomRaces,
    failEarly,
    debug
  )

  def setSelectionSize(n: Int) = MetaParameter(
    populationSize,
    n,
    numberOfGeneration,
    trainRacesNb,
    testRacesNb,
    mutationRate,
    baseConfig,
    defaultConfig,
    notRandom,
    randomRaces,
    failEarly,
    debug
  )

  def setMutationRate(r: Double) = MetaParameter(
    populationSize,
    selectionSize,
    numberOfGeneration,
    trainRacesNb,
    testRacesNb,
    r,
    baseConfig,
    defaultConfig,
    notRandom,
    randomRaces,
    failEarly,
    debug
  )
}

case class Environment(param: MetaParameter, races: Seq[Race]) {
  def judge = JudgeSimulation
  def nextEnvironment =
    if (param.randomRaces) Environment(races.size, param) else this
  def debug(player: Player, racesNb: Int): Unit = {
    val gameSeq = Environment(racesNb, param).games(player)
    val victories = gameSeq.count(g => g.winnerIsPlayerA)
    val failures = gameSeq.count(g => g.winnerIsPlayerB)
    val draw = gameSeq.count(g => (!g.winnerIsPlayerA) && (!g.winnerIsPlayerB))
    Print(s"Victories: $victories")
    Print(s"Failures: $failures")
    Print(s"Draw: $draw")
  }
  def games(player: Player) = races.map { race =>
    {
      Game(race, player, param.defaultPlayer, judge, 0).play
    }
  }
  def fitnessWinner(race: Race): Double = {
    1.0 + race.enemies.map(p => p.distanceToFinish).min / race.raceDistance
  }
  def fitnessLoser(race: Race): Double = -fitnessWinner(race.inverted)

  def fitness(player: Player): Double = {
    if (param.failEarly) {
      val lost = Game(
        races.head,
        player,
        Pilot1Player()(param.baseConfig),
        judge,
        0
      ).play.winnerIsPlayerB
      if (lost) return -1.0
    }
    // use sum(-log(race.step)) to maximize
    games(player)
      .map(g =>
        if (g.winnerIsPlayerA) fitnessWinner(g.race)
        else if (g.winnerIsPlayerB) fitnessLoser(g.race)
        else 0
      )
      .sum / races.size
  }
}

object Environment {
  def apply(racesNb: Int, param: MetaParameter): Environment = {
    Environment(param, for (i <- 1 to racesNb) yield Race.random)
  }
}

case class Individual(config: Config, fitness: Double) {
  def fitness(environment: Environment) = environment.fitness(player)
  def player = MetaPlayer(config)
}

object Individual {
  def apply(config: Config, environment: Environment): Individual = {
    Individual(config, environment.fitness(MetaPlayer(config)))
  }
}

case class Population(p: Seq[Individual]) {
  def leader: Individual = p.maxBy(_.fitness)
  private def selectPopulation(take: Int): Population = Population(
    p.sortBy(-_.fitness).take(take)
  )
  private def randomIndividual = p(scala.util.Random.nextInt(p.size))
  private def updateFitness(e: Environment) = Population(
    p.map(i => Individual(i.config, i.fitness(e)))
  )
  private def breedPopulation(expectedSize: Int, e: Environment): Population = {
    val list = (1 to (expectedSize - p.size))
    val parList = if (e.param.notRandom) list else list.par
    val newGeneration = parList.map(i => breedIndividual(e))
    val newPopulation = Population(updateFitness(e).p ++ newGeneration.toSeq)
    if (e.param.debug) e.debug(newPopulation.leader.player, 10)
    newPopulation
  }
  private def breedIndividual(e: Environment) = {
    val newConfig = randomIndividual.config
      .mergeWith(randomIndividual.config)
      .mutate(e.param.mutationRate)
    Individual(newConfig, e)
  }

  // 1. select best-fit individuals
  // 2. breed new individuals through cross-over and mutations
  // 3. evaluate fitness of new individuals (population)
  def evolve(e: Environment): Population = {
    if (e.param.randomRaces)
      selectPopulation(e.param.selectionSize)
        .updateFitness(e)
        .breedPopulation(p.size, e)
    else
      selectPopulation(e.param.selectionSize).breedPopulation(p.size, e)
  }

  def fitness(e: Environment): Double = leader.fitness(e)
}

case class Experiment(p: Population, e: Environment) {
  def stream: Stream[Population] = {
    def evolve: Population = p.evolve(e)
    val nextPopulation = evolve
    Stream.cons(
      nextPopulation,
      Experiment(nextPopulation, e.nextEnvironment).stream
    )
  }
  def generations: Stream[Population] = stream.take(e.param.numberOfGeneration)
  def fitness: Double = p.fitness(e)
}

case class Plot(time: Seq[Double], train: Seq[Double], test: Seq[Double]) {
  def append(t1: Double, tr: Double, te: Double): Plot = {
    Plot(time :+ t1, train :+ tr, test :+ te)
  }
  def plot(): Plot = {
    import org.sameersingh.scalaplot.Implicits._
    import org.sameersingh.scalaplot._
    val generation = time.size - 1
    val series = new MemXYSeries(time, train, "Training")
    var d: XYData = new XYData(series)
    d += new MemXYSeries(time, test, "Testing")
    val xy = xyChart(
      d,
      title = s"Fitness after $generation generations",
      x = Axis(label = "time"),
      y = Axis(label = "Fitness", range = Some((-1.0, 1.0))),
      legendPosX = LegendPosX.Left,
      legendPosY = LegendPosY.Bottom,
      showLegend = true
    )
    val s: String = output(ASCII, xy)
    Print(s)
    this
  }
}

case class Simulation(
    population: Population,
    trainEnv: Environment,
    testEnv: Environment
) {

  def now(): Double = System.nanoTime() / 1000000000.0
  val t0 = now()

  def draw(plot: Plot, p: Population): Plot = {
    val t1 = now() - t0
    val trainFitness = p.fitness(trainEnv)
    val testFitness = p.fitness(testEnv)
    Print(s"time: $t1, fitness: train=$trainFitness / test=$testFitness")
    plot.append(t1, trainFitness, testFitness).plot()
  }

  def run = {
    val generations: Stream[Population] =
      Experiment(population, trainEnv).generations
    generations.foldLeft(Plot(List(0), List(0), List(0))) {
      case (plot: Plot, p: Population) => {
        draw(plot, p)
      }
    }
    Print(generations.last.leader.config)
  }
}

object Simulation extends App {

  def apply(param: MetaParameter): Simulation = {
    val trainEnv = Environment(param.trainRacesNb, param)
    val testEnv = Environment(param.testRacesNb, param)

    def population: Population = {
      def newIndividual(e: Environment) =
        Individual(param.baseConfig.randomize, e)
      def defaultIndividual(e: Environment) = Individual(param.baseConfig, e)
      val list = (1 to param.populationSize)
      val parList = if (param.notRandom) list else list.par
      val individuals = parList.map(i => newIndividual(trainEnv)).toList
      Population(defaultIndividual(trainEnv) +: individuals)
    }

    Simulation(population, trainEnv, testEnv)
  }

  def makeParams(param: MetaParameter): List[MetaParameter] = {
    param :: param.setPopulation(param.populationSize * 2) ::
      param.setPopulation(param.populationSize / 2) ::
      param.setSelectionSize(param.selectionSize * 2) ::
      param.setSelectionSize(param.selectionSize / 2) ::
      param.setMutationRate(param.mutationRate * 2) ::
      param.setMutationRate(param.mutationRate / 2) :: List()
  }

  val param = MetaParameter(
    populationSize = 10,
    selectionSize = 2,
    numberOfGeneration = 20,
    trainRacesNb = 400,
    testRacesNb = 100,
    mutationRate = 0.01,
    baseConfig = DefaultConfig,
    defaultConfig = DefaultConfig,
    notRandom = false,
    randomRaces = true,
    failEarly = true,
    debug = false
  )

  if (param.notRandom) scala.util.Random.setSeed(1)

  Simulation(param).run
}
