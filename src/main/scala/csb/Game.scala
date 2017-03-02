package csb

import csb.player.Player
import csb.player.Pod
import csb.player.Angle
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

class Game {

  // 0- set initial position of checkpoints and pods
  val laps = 3
  val checkpoints: List[Point] = initCheckpoints
  var pods: Seq[Pod] = initPods

  def initCheckpoints() =
        List(Pixel(304, 138), Pixel(220, 401), Pixel(725, 126), Pixel(696, 390)).map { p => p.toPoint }

  val players = for (i <- 0 to 2) yield Player(checkpoints.toList, laps)

  def step() = {
          val command0 = players(0).update(pods.toList)
          val command1 = players(1).update((pods.slice(2, 4).toList ::: pods.slice(0, 2).toList))
          pods = updatePods(command0 ::: command1)
  }

  def podsTeamA = pods.slice(0, 2)
  def podsTeamB = pods.slice(2, 4)

  def plot(renderer: dom.CanvasRenderingContext2D) = {
      val imgPodA = document.getElementById("podA").asInstanceOf[dom.raw.HTMLImageElement]
      val imgPodB = document.getElementById("podB").asInstanceOf[dom.raw.HTMLImageElement]
      podsTeamA.foreach(plotPod(_, renderer, imgPodA))
      podsTeamB.foreach(plotPod(_, renderer, imgPodB))
  }

  def plotPod(pod: Pod, renderer: dom.CanvasRenderingContext2D, image: dom.raw.HTMLImageElement) = {
    
    val angle = - pod.orientation.radianWith(Point(1, 0)).radian
    val width = image.naturalWidth
    val height = image.naturalHeight
    val pix = Pixel.fromPoint(pod.position)
    renderer.translate(pix.x, pix.y)
    renderer.rotate(angle)
    renderer.translate(- (width / 2), - (height / 2))
    renderer.drawImage(image, 0, 0)
    renderer.setTransform(1, 0, 0, 1, 0, 0)
  }
 
  def initPods = for (i <- 0 to 4) yield Pod(checkpoints(0), checkpoints(1), Angle(0), Point(0, 0))
  def updatePods(commands: Seq[Tuple2[Point,Double]]): Seq[Pod] = {
    pods.toList.zip(commands).map {
      case (pod: Pod, (direction: Point, thrust: Double)) =>
        pod.updateDirection(direction).updateSpeed(thrust).updatePosition
    }
  }
}

@JSExport
object Game {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    var count = 0

    val game = new Game()

    var p = Point(0, 0)
    val corners = Seq(Point(255, 255), Point(0, 255), Point(128, 0))

    def clear() = {
      renderer.clearRect(0, 0, Screen.width, Screen.height)
    }

    def run = {
      clear()
      game.plot(renderer)
      game.step()
    }

    dom.window.setInterval(() => run, 50)
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
  def plot() = sprite.displayAt(Pixel.fromPoint(pod.position), /* FIXME */ 0)
}

case class Frame(time: Int, actors: Seq[Actor]) {
  def plot = actors.foreach(_.plot)
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
  def start() = {
    isPlaying = false
    Timer(timeline.delay, () => if (isPlaying) step())
  }

  def pause() = { isPlaying = false }
  def step() = timeline.setTime(timeline.getTime + timeline.delay)
  def begining() = timeline.setTime(0)
  def end() = timeline.setTime(timeline.duration)
}
