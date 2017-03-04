package csb

import csb.player.Command
import csb.player.Degree
import csb.player.Player
import csb.player.Pod
import csb.player.Point

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.document
import scala.util.Random

object Screen {
    def width = 860
    def height = 484
}

// Board holds the content to be display
object Board {
    def width = 16000
    def height = 9000
    def canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
    val renderer = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]
    def clear() = renderer.clearRect(0, 0, Screen.width, Screen.height)

    def sprite(name: String) = Sprite(document.getElementById(name).asInstanceOf[dom.raw.HTMLImageElement], renderer)
    val sprites: List[Sprite] = List(sprite("podA"), sprite("podA"), sprite("podB"), sprite("podB"))

    def logElement = document.getElementById("terminal").asInstanceOf[html.Div]
}

object Pixel {
    def fromPoint(p: Point): Pixel =
          Pixel((p.x / Board.width * Screen.width).toInt, (-p.y / Board.height * Screen.height).toInt)
}

// Pixel is the coordinate system for the Board
case class Pixel(x: Int, y: Int) {
    def toPoint = Point(x * Board.width / Screen.width, - y * Board.width / Screen.width)
}

// Race holds the state of the simulation
class Race {
  val laps = 3
  val checkpoints: List[Point] = initCheckpoints
  var pods: Seq[Pod] = initPods

  def initCheckpoints() =
        List(Pixel(304, 138), Pixel(220, 401), Pixel(725, 126), Pixel(696, 390)).map { p => p.toPoint }

  val players = for (i <- 0 to 2) yield Player(checkpoints.toList, laps)

  var count = 0
  def hasNextStep: Boolean = (count < 10) // FIXME: find better terminaison condition

  def step() = {
        count += 1
        val commands = players(0).commands(pods.toList) :::
            players(1).commands((pods.slice(2, 4).toList ::: pods.slice(0, 2).toList))
        pods = updatePods(commands)
  }

  def podsTeamA = pods.slice(0, 2)
  def podsTeamB = pods.slice(2, 4)

  def initPods = {
    val departLine = (checkpoints(1) - checkpoints(0)).rotate(Degree(90)).normalize
    val positions = List(1, 3, -1, -3).map(pos => checkpoints(0) + departLine * (pos * 450))
    positions.map(p => Pod(p, checkpoints(1), Degree(0), Point(0, 0)))
  }

  def updatePods(commands: Seq[Command]): Seq[Pod] = {
    pods.toList.zip(commands).map {
      case (pod: Pod, Command(direction, thrust)) =>
        pod.updateDirection(direction).updateSpeed(thrust).updatePosition
    }
  }
  
  def logs: String = "step number " + count
}

// Game connect the simulation with the board
class Game() {

  val race = new Race()

  val animation = new Animation(FrameTimeline(frames))

  val controller = new WindowController(animation)

  def terminal = new Terminal(Board.logElement)
  def podActors = race.pods.zip(Board.sprites).map{case (pod, sprite) => PodActor(pod, sprite)}
  def actors = LogActor(race.logs, terminal) +: podActors

  def frames: Stream[Frame] = {
      if (race.count == 0) {
        val head = Frame(race.count, actors)
        race.step()
        if (race.hasNextStep) head #:: Frame(race.count, actors) #:: frames
        else head #:: Frame(race.count, actors) #:: Stream.empty
      } else {
        race.step()
        if (race.hasNextStep) Frame(race.count, actors) #:: frames
        else Frame(race.count, actors) #:: Stream.empty
    }
  }

  def run() = animation.play()
}

@JSExport
object Game {
  @JSExport
  def main(canvas: html.Canvas): Unit = {

    val game = new Game()
    game.run()
  }
}

case class Sprite (image: dom.raw.HTMLImageElement, renderer: dom.CanvasRenderingContext2D) {
  def displayAt(position: Pixel, angle: Double) = {
    val width = image.naturalWidth
    val height = image.naturalHeight
    renderer.translate(position.x, position.y)
    renderer.rotate(angle)
    renderer.translate(- (width / 2), - (height / 2))
    renderer.drawImage(image, 0, 0)
    renderer.setTransform(1, 0, 0, 1, 0, 0)
  }
}

trait Actor {
  def plot()
}

case class Terminal(element: html.Div) {
  def print(message: String) = element.innerHTML = message
}

case class LogActor(message: String, term: Terminal) extends Actor {
  def plot() = term.print(message)
}

case class PodActor(pod: Pod, sprite: Sprite) extends Actor {
  def plot() = {
    val angle = - pod.orientation.radianWith(Point(1, 0)).radian
    val pix = Pixel.fromPoint(pod.position)
    sprite.displayAt(pix, angle)
  }
}

case class Frame(time: Int, actors: Seq[Actor]) {
  def plot = {
    Board.clear()
    actors.foreach(_.plot)
  }
}

trait Timeline {
  def begining: Int // returns the first
  def nextTime: Int // returns the last if none
  def previousTime: Int // returns the privious time or the first
  def setTime(t: Int) // returns 0 if none
  def computeDuration(): Int // returns total duration
}

case class FrameTimeline(frames: Stream[Frame]) extends Timeline {
  var time = begining

  def begining = frames.head.time
  // warning: this consume the stream
  def computeDuration(): Int = frames.last.time

  def nextTime: Int = frames.find(f => f.time > time) match {
      case Some(f) => f.time
      case None => computeDuration()
  }
  def previousTime: Int = if (time == 0) 0
      else frames.takeWhile(f => f.time < time).last.time
  
  def frame: Frame = frames.find(f => f.time == time) match {
    case Some(f) => f
    case None => {
      Console.err.println("ERROR: frame not found: ", time)
      frames.head
    }
  }

  def setTime(t: Int) = {
      if (t >= frames.head.time) {
      time = t
      frame.plot
    }
  }
}

case class Timer(ms: Double, event: () => Unit ) {
  dom.window.setTimeout(event, ms)
}

case class Animation(timeline: Timeline) {
  var isPlaying = false
  def fps = 10
  def loop() = {
    Timer(50, () => if (isPlaying) {
        if (timeline.nextTime != 0) {
          timeline.setTime(timeline.nextTime)
          play()
        } else {
          isPlaying = false
        }
    })
  }
  def play(): Unit = {
    isPlaying = true
    loop()
  }

  def pause() = { isPlaying = false }
  def playSwitch() = if (isPlaying) pause() else play()

  def jumpTo(t: Int) = {
    if (isPlaying) pause()
    timeline.setTime(t)
  }

  def begining() = jumpTo(timeline.begining)
  def end() = jumpTo(timeline.computeDuration())

  def stepBackward() = jumpTo(timeline.previousTime)
  def stepForward() = jumpTo(timeline.nextTime)
}

class WindowController(animation: Animation) {
  def keypress(event: dom.raw.KeyboardEvent) = event.key match {
    case "PageUp" =>animation.begining()
    case "PageDown" => animation.end()
    case "ArrowLeft" =>animation.stepBackward()
    case "ArrowRight" => animation.stepForward()
    case " " => animation.playSwitch()
  }
  dom.window.addEventListener[dom.raw.KeyboardEvent]("keypress", keypress _)
}

