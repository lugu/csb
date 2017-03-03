package csb

import csb.player.Player
import csb.player.Pod
import csb.player.Angle
import csb.player.Point
import csb.player.Command

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.document
import scala.util.Random

object Screen {
    def width = 860
    def height = 484
}

object Board {
    def width = 16000
    def height = 9000
}

object Pixel {
    def fromPoint(p: Point): Pixel =
          Pixel((p.x / Board.width * Screen.width).toInt, (-p.y / Board.height * Screen.height).toInt)
}

case class Pixel(x: Int, y: Int) {
    def toPoint = Point(x * Board.width / Screen.width, - y * Board.width / Screen.width)
}

class Race {
  val laps = 3
  val checkpoints: List[Point] = initCheckpoints
  var pods: Seq[Pod] = initPods

  def initCheckpoints() =
        List(Pixel(304, 138), Pixel(220, 401), Pixel(725, 126), Pixel(696, 390)).map { p => p.toPoint }

  val players = for (i <- 0 to 2) yield Player(checkpoints.toList, laps)

  var count = 0
  def hasNextStep: Boolean = (count == 100) // FIXME: find better terminaison condition

  def step() = {
        count += 1

        val commands = players(0).commands(pods.toList) :::
            players(1).commands((pods.slice(2, 4).toList ::: pods.slice(0, 2).toList))
        pods = updatePods(commands)
  }

  def podsTeamA = pods.slice(0, 2)
  def podsTeamB = pods.slice(2, 4)

  def initPods = for (i <- 0 to 4) yield Pod(checkpoints(0), checkpoints(1), Angle(0), Point(0, 0))
  def updatePods(commands: Seq[Command]): Seq[Pod] = {
    pods.toList.zip(commands).map {
      case (pod: Pod, Command(direction, thrust)) =>
        pod.updateDirection(direction).updateSpeed(thrust).updatePosition
    }
  }

}

class Game(renderer: dom.CanvasRenderingContext2D) {

  val race = new Race()

  def sprite(name: String) = Sprite(document.getElementById(name).asInstanceOf[dom.raw.HTMLImageElement], renderer)
  val sprites: List[Sprite] = List(sprite("podA"), sprite("podA"), sprite("podB"), sprite("podB"))

  val animation = new Animation(Timeline(frames))

  def raceActors = race.pods.zip(sprites).map{case (pod, sprite) => PodActor(pod, sprite)}

  def frames: Stream[Frame] = {
      race.step()
      val next = if (race.hasNextStep) frames else Stream.empty
      Frame(race.count, raceActors) #:: next
  }

  def run() = animation.play()
}

@JSExport
object Game {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    val game = new Game(renderer)
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

case class PodActor(pod: Pod, sprite: Sprite) extends Actor {
  def plot() = {
    val angle = - pod.orientation.radianWith(Point(1, 0)).radian
    val pix = Pixel.fromPoint(pod.position)
    sprite.displayAt(pix, angle)
  }
}

case class Frame(time: Int, actors: Seq[Actor]) {
  def plot = {
    // FIXME: need to clear the screen before each frame.
    // renderer.clearRect(0, 0, Screen.width, Screen.height)
    actors.foreach(_.plot)
  }
}

case class Timeline(frames: Seq[Frame]) {
  var time: Int = 0

  def delay: Int = getTime - frames.filter(f => f.time > time).head.time
  
  def at(i: Int): Frame = frames.filter(f => f.time >= i).last

  def setTime(t: Int) = {
    time = t
    at(time).plot
  }
  def getTime: Int = time

  def duration: Int = frames.last.time
}

case class Timer(delay: Int, event: () => Unit ) {
  dom.window.setInterval(event, delay)
}

case class Animation(timeline: Timeline) {
  var isPlaying = false
  def play() = {
    isPlaying = false
    Timer(timeline.delay, () => if (isPlaying) step())
  }

  def pause() = { isPlaying = false }
  def step() = timeline.setTime(timeline.getTime + timeline.delay)
  def begining() = timeline.setTime(0)
  def end() = timeline.setTime(timeline.duration)
}
