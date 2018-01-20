package csb

import csb.player.Command
import csb.player.RaceRecord
import csb.player.Player
import csb.player.Pod
import csb.player.Point
import csb.player.Print
import csb.player.Race

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExportTopLevel
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

  val terminals = {
    val tags = List("terminalA", "terminalB", "terminal0")
    tags.map(t => new Terminal(document.getElementById(t).asInstanceOf[html.Span]))
  }
}

object Pixel {
  def fromPoint(p: Point): Pixel =
    Pixel((p.x / Board.width * Screen.width).toInt, (-p.y / Board.height * Screen.height).toInt)
}

// Pixel is the coordinate system for the Board
case class Pixel(x: Int, y: Int) {
  def toPoint = Point(x * Board.width / Screen.width, -y * Board.width / Screen.width)
}

object Logger {
  val default = new Logger()
  Logger.default.makeDefault()
}

class Logger {
  var data: List[String] = List()
  def flush: List[String] = {
    val ret = data
    data = List()
    ret
  }
  def print(msg: String): Unit = {
    data = data :+ msg
  }
  def makeDefault() = Print.setPrinter { message => print(message) }
}

// Game connect the simulation with the board
class Game() {

  var count = 0
  var race = new Race(initCheckpoints, 3)
  var recorder = RaceRecord(race.laps, race.checkpoints, List())

  val loggers = List(new Logger(), new Logger(), Logger.default)

  def loggerA = loggers(0)
  def loggerB = loggers(1)

  val animation = new Animation(FrameTimeline(frames))
  val controller = new WindowController(animation)

  def podActors = race.pods.zip(Board.sprites).map{ case (pod, sprite) => PodActor(pod, sprite) }
  def logActors = loggers.zip(Board.terminals).map{ case (l, t) => LogActor(l.flush, t) }
  def actors = logActors ::: podActors

  def commands = {
    val cmds = {
      loggerA.makeDefault()
      Player(race).commands
    } ::: {
      loggerB.makeDefault()
      Player(race.inverted).commands
    }
    Logger.default.makeDefault()
    cmds
  }

  def initCheckpoints() =
    List(Pixel(304, 138), Pixel(220, 401), Pixel(725, 126), Pixel(696, 390)).map { p => p.toPoint }

  def isFinished: Boolean = (count > 250) // FIXME: find better terminaison condition

  def step() = {
    count += 1
    Print("race step: " + count)
    race.pods.foreach(Print(_))
    race = race.simulate(commands)
    recorder = recorder.updateWith(race, commands.map(c => Some(c)))
    if (isFinished) recorder.dump()
  }

  def frames: Stream[Frame] = {
    if (count == 0) {
      race.pods.foreach(Print(_))
      val head = Frame(count, actors)
      step()
      if (!isFinished) head #:: Frame(count, actors) #:: frames
      else head #:: Frame(count, actors) #:: Stream.empty
    }
    else {
      step()
      if (!isFinished) Frame(count, actors) #:: frames
      else Frame(count, actors) #:: Stream.empty
    }
  }

  def run() = animation.play()
}

object Game {
  @JSExportTopLevel("csb.Game.main")
  def main(canvas: html.Canvas): Unit = {
    val game = new Game()
    game.run()
  }
}

case class Sprite(image: dom.raw.HTMLImageElement, renderer: dom.CanvasRenderingContext2D) {
  def displayAt(position: Pixel, angle: Double) = {
    val width = image.naturalWidth
    val height = image.naturalHeight
    renderer.translate(position.x, position.y)
    renderer.rotate(angle)
    renderer.translate(-(width / 2), -(height / 2))
    renderer.drawImage(image, 0, 0)
    renderer.setTransform(1, 0, 0, 1, 0, 0)
  }
}

trait Actor {
  def plot()
}

case class Terminal(element: html.Span) {
  def clear = {
    import scalatags.JsDom.all._
    while (element.hasChildNodes) element.removeChild(element.firstChild)
    element.appendChild(ul().render)
  }
  def format(message: String) = {
  }
  def print(message: String) = {
    import scalatags.JsDom.all._
    message.split("\n").foreach(m => element.firstChild.appendChild(li(m).render))
  }
}

case class LogActor(messages: List[String], term: Terminal) extends Actor {
  def plot() = {
    term.clear
    messages.foreach(term.print)
  }
}

case class PodActor(pod: Pod, sprite: Sprite) extends Actor {
  def plot() = {
    val angle = -pod.orientation.radianWith(Point(1, 0)).radian
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
    case None    => computeDuration()
  }
  def previousTime: Int = if (time == 0) 0
  else frames.takeWhile(f => f.time < time).last.time

  def frame: Frame = frames.find(f => f.time == time) match {
    case Some(f) => f
    case None => {
      Print("ERROR: frame not found: ", time.toString)
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

case class Timer(ms: Double, event: () => Unit) {
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
      }
      else {
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
    case "PageUp"     => animation.begining(); event.preventDefault()
    case "PageDown"   => animation.end(); event.preventDefault()
    case "ArrowLeft"  => animation.stepBackward(); event.preventDefault()
    case "ArrowRight" => animation.stepForward(); event.preventDefault()
    case " "          => animation.playSwitch(); event.preventDefault()
  }
  dom.window.addEventListener[dom.raw.KeyboardEvent]("keydown", keypress _)
}

