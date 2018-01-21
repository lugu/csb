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

object OnlinePlayer extends App {

  val Array(laps) = for (i ← Input() split " ") yield i.toInt
  val Array(checkpointNb) = for (i ← Input() split " ") yield i.toInt
  val checkpoints: List[Point] = (for (i ← 1 to checkpointNb) yield {
    val Array(checkpointX, checkpointY) = for (i ← Input() split " ") yield i.toInt
    // reverse Y coordinate as the input are non cartesian
    Point(checkpointX, -checkpointY)
  }).toList

  val destinations = (for (i ← 1 to checkpointNb) yield checkpoints.tail :+ checkpoints.head).flatten.toList

  var pods: List[Pod] = (for (i ← 1 to 4) yield {
    val u = PodUpdate(checkpoints)
    Pod(u.position, destinations, u.orientation, u.speed, true)
  }).toList

  var race = Race(pods, checkpoints, laps)

  var recorder = RaceRecord(laps, checkpoints, List())

  val player = new MetaPlayer()

  while (!race.isFinished) {
    val commands = player.commands(race)

    recorder = recorder.updateWith(race, List(Some(commands(0)), Some(commands(1)), None, None))
    // IO.dump()

    if (! race.isFinished) {
      commands.foreach(c => Output(c.answer))
      pods = for (p ← pods) yield { p.updateWith(PodUpdate(checkpoints)) }
      race = Race(pods, checkpoints, laps)
    }
  }
}

object Print {
  var printer: (String) ⇒ Unit = Console.err.println(_)
  def setPrinter(p: String ⇒ Unit) = {
    printer = p
  }
  def apply(messages: Object*) = {
    printer(messages.mkString(" "))
  }
}

object Input {
  def apply(): String = IO.readLine()
}

object Output {
  def apply(s: String) = IO.println(s)
}

object IO {

  var record = ""

  def println(out: String) = {
    record += out + "\n"
    scala.Console.println(out)
  }

  def readLine(): String = {
    val line = scala.io.StdIn.readLine()
    record += line
    line
  }

  def decompress(inData: Array[Byte]): Array[Byte] = {
    import java.util.zip.Inflater
    val inflater = new Inflater()
    inflater.setInput(inData)
    val decompressedData = new Array[Byte](inData.size * 2)
    var count = inflater.inflate(decompressedData)
    var finalData = decompressedData.take(count)
    while (count > 0) {
      count = inflater.inflate(decompressedData)
      finalData = finalData ++ decompressedData.take(count)
    }
    return finalData 
  }

  def compress(inData: Array[Byte]): Array[Byte] = {
    import java.util.zip.Deflater
    var deflater: Deflater = new Deflater()
    deflater.setInput(inData)
    deflater.finish
    // compressed data can be larger than original data
    val compressedData = new Array[Byte](inData.size * 2)
    val count: Int = deflater.deflate(compressedData)
    return compressedData.take(count)
  }

  def unbase64(inString: String): Array[Byte] = {
      import java.util.Base64
      Base64.getDecoder.decode(inString)
  }

  def base64(inData: Array[Byte]): String = {
      import java.util.Base64
      Base64.getEncoder.encodeToString(inData)
  }

  def dump() = {
    val out = base64(compress(record.getBytes))
    var printed = 0
    while(printed < out.size) {
        Print(out.slice(printed, printed + 1024))
        printed += 1024
    }
  }
}

