package csb

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExportTopLevel

object Screen {
  def width = 860
  def height = 484
}

// Window holds the content to be display
object Window {
  def width = 16000
  def height = 9000
  def canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  val renderer = canvas
    .getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]
  def clear() = renderer.clearRect(0, 0, Screen.width, Screen.height)

  def circle(position: Pixel, radius: Int) = {}

  def sprite(name: String) =
    Sprite(document.getElementById(name).asInstanceOf[dom.raw.HTMLImageElement],
           renderer)
  val sprites: List[Sprite] =
    List(sprite("podA"), sprite("podA"), sprite("podB"), sprite("podB"))

  val terminals = {
    val tags = List("terminalA", "terminalB", "terminal0")
    tags.map(t =>
      new Terminal(document.getElementById(t).asInstanceOf[html.Span]))
  }
}

object Pixel {
  def fromPoint(p: Point): Pixel =
    Pixel((p.x / Window.width * Screen.width).toInt,
          (-p.y / Window.height * Screen.height).toInt)
}

// Pixel is the coordinate system for the Window
case class Pixel(x: Int, y: Int) {
  def toPoint =
    Point(x * Window.width / Screen.width, -y * Window.width / Screen.width)
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
  def makeDefault() = Print.setPrinter { message =>
    print(message)
  }
}

case class PlayerLogger(player: Player, logger: Logger) extends Player {
  def commands(race: Race): List[Move] = {
    logger.makeDefault()
    val c = player.commands(race)
    Logger.default.makeDefault()
    c
  }
}

class Board(game: Game) {

  def podActors = game.race.pods.zip(Window.sprites).map {
    case (pod, sprite) => PodActor(pod, sprite)
  }
  def checkpointActors = game.race.checkpoints.zipWithIndex.map {
    case (p: Point, i: Int) => CheckpointActor(Pixel.fromPoint(p), i.toString)
  }
  def logActors =
    Board.loggers.zip(Window.terminals).map {
      case (l, t) => LogActor(l.flush, t)
    }
  def actors = logActors ::: checkpointActors ::: podActors

  def nextTurn: Board = new Board(game.nextTurn)

  def frames: Stream[Frame] = {
    if (game.step == 0) {
      val head = Frame(game.step, actors)
      if (!game.isFinished)
        head #:: Frame(game.step, actors) #:: nextTurn.frames
      else head #:: Frame(game.step, actors) #:: Stream.empty
    } else {
      if (!game.isFinished) Frame(game.step, actors) #:: nextTurn.frames
      else {
        if (game.race.winnerIsPlayerA) Print("Player A wins")
        else if (game.race.winnerIsPlayerB) Print("Player B wins")
        else Print("Drawn race")
        Frame(game.step, actors) #:: Stream.empty
      }
    }
  }

  def play() = {
    val animation = new Animation(FrameTimeline(frames))
    val controller = new WindowController(animation)
    animation.play()
  }
}

object Board {
  val loggers = List(new Logger(), new Logger(), Logger.default)

  def loggerA = loggers(0)
  def loggerB = loggers(1)

  def apply(game: Game): Board = {
    val playerA = PlayerLogger(game.playerA, loggerA)
    val playerB = PlayerLogger(game.playerB, loggerB)
    new Board(new Game(game.race, playerA, playerB, game.judge, game.step))
  }
}

object Application {

  @JSExportTopLevel("csb.Application.main")
  def main(canvas: html.Canvas): Unit = {
    val game = randomGame
    // val game = csb.races.RaceRecord2.game
    Board(game).play()
  }

  def randomGame = {
    val playerA = new MetaPlayer(BetterConfig)
    val playerB = new MetaPlayer(DefaultConfig)
    val judge = JudgeSimulation()
    new Game(Race.random, playerA, playerB, judge, 0)
  }
}

case class Sprite(image: dom.raw.HTMLImageElement,
                  renderer: dom.CanvasRenderingContext2D) {
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
  def format(message: String) = {}
  def print(message: String) = {
    import scalatags.JsDom.all._
    message
      .split("\n")
      .foreach(m => element.firstChild.appendChild(li(m).render))
  }
}

case class CheckpointActor(pos: Pixel, label: String) extends Actor {
  def context: dom.CanvasRenderingContext2D = Window.renderer
  def plot() = {
    context.beginPath()
    context.arc(pos.x, pos.y, 35, 0, 2 * Math.PI, false)
    context.fillStyle = "#000000"
    context.fill()
    context.lineWidth = 5
    context.strokeStyle = "#ffffff"
    context.stroke()
    context.font = "20px Arial"
    context.lineWidth = 2
    context.strokeStyle = "#ffffff"
    context.strokeText(label, pos.x - 8, pos.y + 8)
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
    Window.clear()
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
  def previousTime: Int =
    if (time == 0) 0
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
    Timer(50,
          () =>
            if (isPlaying) {
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
    case "PageUp"     => animation.begining(); event.preventDefault()
    case "PageDown"   => animation.end(); event.preventDefault()
    case "ArrowLeft"  => animation.stepBackward(); event.preventDefault()
    case "ArrowRight" => animation.stepForward(); event.preventDefault()
    case " "          => animation.playSwitch(); event.preventDefault()
  }
  dom.window.addEventListener[dom.raw.KeyboardEvent]("keydown", keypress _)
}
