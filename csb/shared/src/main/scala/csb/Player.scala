package csb

trait Player {
  def commands(race: Race): List[Command]
}

trait SimplePlayer extends Player {
  def pilot(pod: Pod, race: Race): Pilot
  def commands(race: Race): List[Command] = {
    val pilots: List[Pilot] = List(pilot(race.pod0, race), pilot(race.pod1, race))
    pilots.map(_.command)
  }
}

class MetaPlayer extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = MetaPilot(pod, race)
}

class Player0 extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = Pilot0(pod)
}

class Player1 extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = Pilot1(pod, race)
}

class Player2 extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = Pilot2(pod, race)
}

class PlayerDefense extends SimplePlayer {
  def pilot(pod: Pod, race: Race): Pilot = PilotDefense(pod)
}

