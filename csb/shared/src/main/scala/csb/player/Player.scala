package csb.player

trait Player {
  def commands(race: Race): List[Command]
}

trait PilotContructor {
  def pilot(pod: Pod, race: Race): Pilot
}

class SimplePlayer(val constructor: PilotContructor) extends Player {
  def pilot(pod: Pod, race: Race): Pilot = constructor.pilot(pod, race)
  def commands(race: Race): List[Command] = {
    val pilots: List[Pilot] = List(pilot(race.pod0, race), pilot(race.pod1, race))
    pilots.map(_.command)
  }
}

object MetaPilotConstructor extends PilotContructor {
  def pilot(pod: Pod, race: Race) = MetaPilot(pod, race)
}

class MetaPlayer extends SimplePlayer(MetaPilotConstructor)

